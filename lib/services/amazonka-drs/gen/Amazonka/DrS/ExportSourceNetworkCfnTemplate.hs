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
-- Module      : Amazonka.DrS.ExportSourceNetworkCfnTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Export the Source Network CloudFormation template to an S3 bucket.
module Amazonka.DrS.ExportSourceNetworkCfnTemplate
  ( -- * Creating a Request
    ExportSourceNetworkCfnTemplate (..),
    newExportSourceNetworkCfnTemplate,

    -- * Request Lenses
    exportSourceNetworkCfnTemplate_sourceNetworkID,

    -- * Destructuring the Response
    ExportSourceNetworkCfnTemplateResponse (..),
    newExportSourceNetworkCfnTemplateResponse,

    -- * Response Lenses
    exportSourceNetworkCfnTemplateResponse_s3DestinationUrl,
    exportSourceNetworkCfnTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportSourceNetworkCfnTemplate' smart constructor.
data ExportSourceNetworkCfnTemplate = ExportSourceNetworkCfnTemplate'
  { -- | The Source Network ID to export its CloudFormation template to an S3
    -- bucket.
    sourceNetworkID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportSourceNetworkCfnTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceNetworkID', 'exportSourceNetworkCfnTemplate_sourceNetworkID' - The Source Network ID to export its CloudFormation template to an S3
-- bucket.
newExportSourceNetworkCfnTemplate ::
  -- | 'sourceNetworkID'
  Prelude.Text ->
  ExportSourceNetworkCfnTemplate
newExportSourceNetworkCfnTemplate pSourceNetworkID_ =
  ExportSourceNetworkCfnTemplate'
    { sourceNetworkID =
        pSourceNetworkID_
    }

-- | The Source Network ID to export its CloudFormation template to an S3
-- bucket.
exportSourceNetworkCfnTemplate_sourceNetworkID :: Lens.Lens' ExportSourceNetworkCfnTemplate Prelude.Text
exportSourceNetworkCfnTemplate_sourceNetworkID = Lens.lens (\ExportSourceNetworkCfnTemplate' {sourceNetworkID} -> sourceNetworkID) (\s@ExportSourceNetworkCfnTemplate' {} a -> s {sourceNetworkID = a} :: ExportSourceNetworkCfnTemplate)

instance
  Core.AWSRequest
    ExportSourceNetworkCfnTemplate
  where
  type
    AWSResponse ExportSourceNetworkCfnTemplate =
      ExportSourceNetworkCfnTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportSourceNetworkCfnTemplateResponse'
            Prelude.<$> (x Data..?> "s3DestinationUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ExportSourceNetworkCfnTemplate
  where
  hashWithSalt
    _salt
    ExportSourceNetworkCfnTemplate' {..} =
      _salt `Prelude.hashWithSalt` sourceNetworkID

instance
  Prelude.NFData
    ExportSourceNetworkCfnTemplate
  where
  rnf ExportSourceNetworkCfnTemplate' {..} =
    Prelude.rnf sourceNetworkID

instance
  Data.ToHeaders
    ExportSourceNetworkCfnTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExportSourceNetworkCfnTemplate where
  toJSON ExportSourceNetworkCfnTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceNetworkID" Data..= sourceNetworkID)
          ]
      )

instance Data.ToPath ExportSourceNetworkCfnTemplate where
  toPath =
    Prelude.const "/ExportSourceNetworkCfnTemplate"

instance Data.ToQuery ExportSourceNetworkCfnTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportSourceNetworkCfnTemplateResponse' smart constructor.
data ExportSourceNetworkCfnTemplateResponse = ExportSourceNetworkCfnTemplateResponse'
  { -- | S3 bucket URL where the Source Network CloudFormation template was
    -- exported to.
    s3DestinationUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportSourceNetworkCfnTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DestinationUrl', 'exportSourceNetworkCfnTemplateResponse_s3DestinationUrl' - S3 bucket URL where the Source Network CloudFormation template was
-- exported to.
--
-- 'httpStatus', 'exportSourceNetworkCfnTemplateResponse_httpStatus' - The response's http status code.
newExportSourceNetworkCfnTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportSourceNetworkCfnTemplateResponse
newExportSourceNetworkCfnTemplateResponse
  pHttpStatus_ =
    ExportSourceNetworkCfnTemplateResponse'
      { s3DestinationUrl =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | S3 bucket URL where the Source Network CloudFormation template was
-- exported to.
exportSourceNetworkCfnTemplateResponse_s3DestinationUrl :: Lens.Lens' ExportSourceNetworkCfnTemplateResponse (Prelude.Maybe Prelude.Text)
exportSourceNetworkCfnTemplateResponse_s3DestinationUrl = Lens.lens (\ExportSourceNetworkCfnTemplateResponse' {s3DestinationUrl} -> s3DestinationUrl) (\s@ExportSourceNetworkCfnTemplateResponse' {} a -> s {s3DestinationUrl = a} :: ExportSourceNetworkCfnTemplateResponse)

-- | The response's http status code.
exportSourceNetworkCfnTemplateResponse_httpStatus :: Lens.Lens' ExportSourceNetworkCfnTemplateResponse Prelude.Int
exportSourceNetworkCfnTemplateResponse_httpStatus = Lens.lens (\ExportSourceNetworkCfnTemplateResponse' {httpStatus} -> httpStatus) (\s@ExportSourceNetworkCfnTemplateResponse' {} a -> s {httpStatus = a} :: ExportSourceNetworkCfnTemplateResponse)

instance
  Prelude.NFData
    ExportSourceNetworkCfnTemplateResponse
  where
  rnf ExportSourceNetworkCfnTemplateResponse' {..} =
    Prelude.rnf s3DestinationUrl
      `Prelude.seq` Prelude.rnf httpStatus
