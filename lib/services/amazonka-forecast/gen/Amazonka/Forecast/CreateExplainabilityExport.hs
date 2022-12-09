{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Forecast.CreateExplainabilityExport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports an Explainability resource created by the CreateExplainability
-- operation. Exported files are exported to an Amazon Simple Storage
-- Service (Amazon S3) bucket.
--
-- You must specify a DataDestination object that includes an Amazon S3
-- bucket and an AWS Identity and Access Management (IAM) role that Amazon
-- Forecast can assume to access the Amazon S3 bucket. For more
-- information, see aws-forecast-iam-roles.
--
-- The @Status@ of the export job must be @ACTIVE@ before you can access
-- the export in your Amazon S3 bucket. To get the status, use the
-- DescribeExplainabilityExport operation.
module Amazonka.Forecast.CreateExplainabilityExport
  ( -- * Creating a Request
    CreateExplainabilityExport (..),
    newCreateExplainabilityExport,

    -- * Request Lenses
    createExplainabilityExport_format,
    createExplainabilityExport_tags,
    createExplainabilityExport_explainabilityExportName,
    createExplainabilityExport_explainabilityArn,
    createExplainabilityExport_destination,

    -- * Destructuring the Response
    CreateExplainabilityExportResponse (..),
    newCreateExplainabilityExportResponse,

    -- * Response Lenses
    createExplainabilityExportResponse_explainabilityExportArn,
    createExplainabilityExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateExplainabilityExport' smart constructor.
data CreateExplainabilityExport = CreateExplainabilityExport'
  { -- | The format of the exported data, CSV or PARQUET.
    format :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata to help you categorize and organize your resources.
    -- Each tag consists of a key and an optional value, both of which you
    -- define. Tag keys and values are case sensitive.
    --
    -- The following restrictions apply to tags:
    --
    -- -   For each resource, each tag key must be unique and each tag key must
    --     have one value.
    --
    -- -   Maximum number of tags per resource: 50.
    --
    -- -   Maximum key length: 128 Unicode characters in UTF-8.
    --
    -- -   Maximum value length: 256 Unicode characters in UTF-8.
    --
    -- -   Accepted characters: all letters and numbers, spaces representable
    --     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
    --     across other services and resources, the character restrictions of
    --     those services also apply.
    --
    -- -   Key prefixes cannot include any upper or lowercase combination of
    --     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
    --     @aws@ as its prefix but the key does not, Forecast considers it to
    --     be a user tag and will count against the limit of 50 tags. Tags with
    --     only the key prefix of @aws@ do not count against your tags per
    --     resource limit. You cannot edit or delete tag keys with this prefix.
    tags :: Prelude.Maybe [Tag],
    -- | A unique name for the Explainability export.
    explainabilityExportName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Explainability to export.
    explainabilityArn :: Prelude.Text,
    destination :: DataDestination
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExplainabilityExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'createExplainabilityExport_format' - The format of the exported data, CSV or PARQUET.
--
-- 'tags', 'createExplainabilityExport_tags' - Optional metadata to help you categorize and organize your resources.
-- Each tag consists of a key and an optional value, both of which you
-- define. Tag keys and values are case sensitive.
--
-- The following restrictions apply to tags:
--
-- -   For each resource, each tag key must be unique and each tag key must
--     have one value.
--
-- -   Maximum number of tags per resource: 50.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Accepted characters: all letters and numbers, spaces representable
--     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
--     across other services and resources, the character restrictions of
--     those services also apply.
--
-- -   Key prefixes cannot include any upper or lowercase combination of
--     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
--     @aws@ as its prefix but the key does not, Forecast considers it to
--     be a user tag and will count against the limit of 50 tags. Tags with
--     only the key prefix of @aws@ do not count against your tags per
--     resource limit. You cannot edit or delete tag keys with this prefix.
--
-- 'explainabilityExportName', 'createExplainabilityExport_explainabilityExportName' - A unique name for the Explainability export.
--
-- 'explainabilityArn', 'createExplainabilityExport_explainabilityArn' - The Amazon Resource Name (ARN) of the Explainability to export.
--
-- 'destination', 'createExplainabilityExport_destination' - Undocumented member.
newCreateExplainabilityExport ::
  -- | 'explainabilityExportName'
  Prelude.Text ->
  -- | 'explainabilityArn'
  Prelude.Text ->
  -- | 'destination'
  DataDestination ->
  CreateExplainabilityExport
newCreateExplainabilityExport
  pExplainabilityExportName_
  pExplainabilityArn_
  pDestination_ =
    CreateExplainabilityExport'
      { format =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        explainabilityExportName =
          pExplainabilityExportName_,
        explainabilityArn = pExplainabilityArn_,
        destination = pDestination_
      }

-- | The format of the exported data, CSV or PARQUET.
createExplainabilityExport_format :: Lens.Lens' CreateExplainabilityExport (Prelude.Maybe Prelude.Text)
createExplainabilityExport_format = Lens.lens (\CreateExplainabilityExport' {format} -> format) (\s@CreateExplainabilityExport' {} a -> s {format = a} :: CreateExplainabilityExport)

-- | Optional metadata to help you categorize and organize your resources.
-- Each tag consists of a key and an optional value, both of which you
-- define. Tag keys and values are case sensitive.
--
-- The following restrictions apply to tags:
--
-- -   For each resource, each tag key must be unique and each tag key must
--     have one value.
--
-- -   Maximum number of tags per resource: 50.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Accepted characters: all letters and numbers, spaces representable
--     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
--     across other services and resources, the character restrictions of
--     those services also apply.
--
-- -   Key prefixes cannot include any upper or lowercase combination of
--     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
--     @aws@ as its prefix but the key does not, Forecast considers it to
--     be a user tag and will count against the limit of 50 tags. Tags with
--     only the key prefix of @aws@ do not count against your tags per
--     resource limit. You cannot edit or delete tag keys with this prefix.
createExplainabilityExport_tags :: Lens.Lens' CreateExplainabilityExport (Prelude.Maybe [Tag])
createExplainabilityExport_tags = Lens.lens (\CreateExplainabilityExport' {tags} -> tags) (\s@CreateExplainabilityExport' {} a -> s {tags = a} :: CreateExplainabilityExport) Prelude.. Lens.mapping Lens.coerced

-- | A unique name for the Explainability export.
createExplainabilityExport_explainabilityExportName :: Lens.Lens' CreateExplainabilityExport Prelude.Text
createExplainabilityExport_explainabilityExportName = Lens.lens (\CreateExplainabilityExport' {explainabilityExportName} -> explainabilityExportName) (\s@CreateExplainabilityExport' {} a -> s {explainabilityExportName = a} :: CreateExplainabilityExport)

-- | The Amazon Resource Name (ARN) of the Explainability to export.
createExplainabilityExport_explainabilityArn :: Lens.Lens' CreateExplainabilityExport Prelude.Text
createExplainabilityExport_explainabilityArn = Lens.lens (\CreateExplainabilityExport' {explainabilityArn} -> explainabilityArn) (\s@CreateExplainabilityExport' {} a -> s {explainabilityArn = a} :: CreateExplainabilityExport)

-- | Undocumented member.
createExplainabilityExport_destination :: Lens.Lens' CreateExplainabilityExport DataDestination
createExplainabilityExport_destination = Lens.lens (\CreateExplainabilityExport' {destination} -> destination) (\s@CreateExplainabilityExport' {} a -> s {destination = a} :: CreateExplainabilityExport)

instance Core.AWSRequest CreateExplainabilityExport where
  type
    AWSResponse CreateExplainabilityExport =
      CreateExplainabilityExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExplainabilityExportResponse'
            Prelude.<$> (x Data..?> "ExplainabilityExportArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateExplainabilityExport where
  hashWithSalt _salt CreateExplainabilityExport' {..} =
    _salt `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` explainabilityExportName
      `Prelude.hashWithSalt` explainabilityArn
      `Prelude.hashWithSalt` destination

instance Prelude.NFData CreateExplainabilityExport where
  rnf CreateExplainabilityExport' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf explainabilityExportName
      `Prelude.seq` Prelude.rnf explainabilityArn
      `Prelude.seq` Prelude.rnf destination

instance Data.ToHeaders CreateExplainabilityExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.CreateExplainabilityExport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateExplainabilityExport where
  toJSON CreateExplainabilityExport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Format" Data..=) Prelude.<$> format,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "ExplainabilityExportName"
                  Data..= explainabilityExportName
              ),
            Prelude.Just
              ("ExplainabilityArn" Data..= explainabilityArn),
            Prelude.Just ("Destination" Data..= destination)
          ]
      )

instance Data.ToPath CreateExplainabilityExport where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateExplainabilityExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateExplainabilityExportResponse' smart constructor.
data CreateExplainabilityExportResponse = CreateExplainabilityExportResponse'
  { -- | The Amazon Resource Name (ARN) of the export.
    explainabilityExportArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExplainabilityExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'explainabilityExportArn', 'createExplainabilityExportResponse_explainabilityExportArn' - The Amazon Resource Name (ARN) of the export.
--
-- 'httpStatus', 'createExplainabilityExportResponse_httpStatus' - The response's http status code.
newCreateExplainabilityExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateExplainabilityExportResponse
newCreateExplainabilityExportResponse pHttpStatus_ =
  CreateExplainabilityExportResponse'
    { explainabilityExportArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the export.
createExplainabilityExportResponse_explainabilityExportArn :: Lens.Lens' CreateExplainabilityExportResponse (Prelude.Maybe Prelude.Text)
createExplainabilityExportResponse_explainabilityExportArn = Lens.lens (\CreateExplainabilityExportResponse' {explainabilityExportArn} -> explainabilityExportArn) (\s@CreateExplainabilityExportResponse' {} a -> s {explainabilityExportArn = a} :: CreateExplainabilityExportResponse)

-- | The response's http status code.
createExplainabilityExportResponse_httpStatus :: Lens.Lens' CreateExplainabilityExportResponse Prelude.Int
createExplainabilityExportResponse_httpStatus = Lens.lens (\CreateExplainabilityExportResponse' {httpStatus} -> httpStatus) (\s@CreateExplainabilityExportResponse' {} a -> s {httpStatus = a} :: CreateExplainabilityExportResponse)

instance
  Prelude.NFData
    CreateExplainabilityExportResponse
  where
  rnf CreateExplainabilityExportResponse' {..} =
    Prelude.rnf explainabilityExportArn
      `Prelude.seq` Prelude.rnf httpStatus
