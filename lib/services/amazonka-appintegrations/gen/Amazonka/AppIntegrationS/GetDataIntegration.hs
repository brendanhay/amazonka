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
-- Module      : Amazonka.AppIntegrationS.GetDataIntegration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the DataIntegration.
--
-- You cannot create a DataIntegration association for a DataIntegration
-- that has been previously associated. Use a different DataIntegration, or
-- recreate the DataIntegration using the
-- <https://docs.aws.amazon.com/appintegrations/latest/APIReference/API_CreateDataIntegration.html CreateDataIntegration>
-- API.
module Amazonka.AppIntegrationS.GetDataIntegration
  ( -- * Creating a Request
    GetDataIntegration (..),
    newGetDataIntegration,

    -- * Request Lenses
    getDataIntegration_identifier,

    -- * Destructuring the Response
    GetDataIntegrationResponse (..),
    newGetDataIntegrationResponse,

    -- * Response Lenses
    getDataIntegrationResponse_arn,
    getDataIntegrationResponse_description,
    getDataIntegrationResponse_fileConfiguration,
    getDataIntegrationResponse_id,
    getDataIntegrationResponse_kmsKey,
    getDataIntegrationResponse_name,
    getDataIntegrationResponse_objectConfiguration,
    getDataIntegrationResponse_scheduleConfiguration,
    getDataIntegrationResponse_sourceURI,
    getDataIntegrationResponse_tags,
    getDataIntegrationResponse_httpStatus,
  )
where

import Amazonka.AppIntegrationS.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataIntegration' smart constructor.
data GetDataIntegration = GetDataIntegration'
  { -- | A unique identifier.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'getDataIntegration_identifier' - A unique identifier.
newGetDataIntegration ::
  -- | 'identifier'
  Prelude.Text ->
  GetDataIntegration
newGetDataIntegration pIdentifier_ =
  GetDataIntegration' {identifier = pIdentifier_}

-- | A unique identifier.
getDataIntegration_identifier :: Lens.Lens' GetDataIntegration Prelude.Text
getDataIntegration_identifier = Lens.lens (\GetDataIntegration' {identifier} -> identifier) (\s@GetDataIntegration' {} a -> s {identifier = a} :: GetDataIntegration)

instance Core.AWSRequest GetDataIntegration where
  type
    AWSResponse GetDataIntegration =
      GetDataIntegrationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataIntegrationResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "FileConfiguration")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "KmsKey")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> ( x
                            Data..?> "ObjectConfiguration"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ScheduleConfiguration")
            Prelude.<*> (x Data..?> "SourceURI")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataIntegration where
  hashWithSalt _salt GetDataIntegration' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData GetDataIntegration where
  rnf GetDataIntegration' {..} = Prelude.rnf identifier

instance Data.ToHeaders GetDataIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDataIntegration where
  toPath GetDataIntegration' {..} =
    Prelude.mconcat
      ["/dataIntegrations/", Data.toBS identifier]

instance Data.ToQuery GetDataIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataIntegrationResponse' smart constructor.
data GetDataIntegrationResponse = GetDataIntegrationResponse'
  { -- | The Amazon Resource Name (ARN) for the DataIntegration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The KMS key for the DataIntegration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The configuration for what files should be pulled from the source.
    fileConfiguration :: Prelude.Maybe FileConfiguration,
    -- | A unique identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The KMS key for the DataIntegration.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the DataIntegration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The configuration for what data should be pulled from the source.
    objectConfiguration :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text))),
    -- | The name of the data and how often it should be pulled from the source.
    scheduleConfiguration :: Prelude.Maybe ScheduleConfiguration,
    -- | The URI of the data source.
    sourceURI :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getDataIntegrationResponse_arn' - The Amazon Resource Name (ARN) for the DataIntegration.
--
-- 'description', 'getDataIntegrationResponse_description' - The KMS key for the DataIntegration.
--
-- 'fileConfiguration', 'getDataIntegrationResponse_fileConfiguration' - The configuration for what files should be pulled from the source.
--
-- 'id', 'getDataIntegrationResponse_id' - A unique identifier.
--
-- 'kmsKey', 'getDataIntegrationResponse_kmsKey' - The KMS key for the DataIntegration.
--
-- 'name', 'getDataIntegrationResponse_name' - The name of the DataIntegration.
--
-- 'objectConfiguration', 'getDataIntegrationResponse_objectConfiguration' - The configuration for what data should be pulled from the source.
--
-- 'scheduleConfiguration', 'getDataIntegrationResponse_scheduleConfiguration' - The name of the data and how often it should be pulled from the source.
--
-- 'sourceURI', 'getDataIntegrationResponse_sourceURI' - The URI of the data source.
--
-- 'tags', 'getDataIntegrationResponse_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'httpStatus', 'getDataIntegrationResponse_httpStatus' - The response's http status code.
newGetDataIntegrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataIntegrationResponse
newGetDataIntegrationResponse pHttpStatus_ =
  GetDataIntegrationResponse'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      fileConfiguration = Prelude.Nothing,
      id = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      name = Prelude.Nothing,
      objectConfiguration = Prelude.Nothing,
      scheduleConfiguration = Prelude.Nothing,
      sourceURI = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the DataIntegration.
getDataIntegrationResponse_arn :: Lens.Lens' GetDataIntegrationResponse (Prelude.Maybe Prelude.Text)
getDataIntegrationResponse_arn = Lens.lens (\GetDataIntegrationResponse' {arn} -> arn) (\s@GetDataIntegrationResponse' {} a -> s {arn = a} :: GetDataIntegrationResponse)

-- | The KMS key for the DataIntegration.
getDataIntegrationResponse_description :: Lens.Lens' GetDataIntegrationResponse (Prelude.Maybe Prelude.Text)
getDataIntegrationResponse_description = Lens.lens (\GetDataIntegrationResponse' {description} -> description) (\s@GetDataIntegrationResponse' {} a -> s {description = a} :: GetDataIntegrationResponse)

-- | The configuration for what files should be pulled from the source.
getDataIntegrationResponse_fileConfiguration :: Lens.Lens' GetDataIntegrationResponse (Prelude.Maybe FileConfiguration)
getDataIntegrationResponse_fileConfiguration = Lens.lens (\GetDataIntegrationResponse' {fileConfiguration} -> fileConfiguration) (\s@GetDataIntegrationResponse' {} a -> s {fileConfiguration = a} :: GetDataIntegrationResponse)

-- | A unique identifier.
getDataIntegrationResponse_id :: Lens.Lens' GetDataIntegrationResponse (Prelude.Maybe Prelude.Text)
getDataIntegrationResponse_id = Lens.lens (\GetDataIntegrationResponse' {id} -> id) (\s@GetDataIntegrationResponse' {} a -> s {id = a} :: GetDataIntegrationResponse)

-- | The KMS key for the DataIntegration.
getDataIntegrationResponse_kmsKey :: Lens.Lens' GetDataIntegrationResponse (Prelude.Maybe Prelude.Text)
getDataIntegrationResponse_kmsKey = Lens.lens (\GetDataIntegrationResponse' {kmsKey} -> kmsKey) (\s@GetDataIntegrationResponse' {} a -> s {kmsKey = a} :: GetDataIntegrationResponse)

-- | The name of the DataIntegration.
getDataIntegrationResponse_name :: Lens.Lens' GetDataIntegrationResponse (Prelude.Maybe Prelude.Text)
getDataIntegrationResponse_name = Lens.lens (\GetDataIntegrationResponse' {name} -> name) (\s@GetDataIntegrationResponse' {} a -> s {name = a} :: GetDataIntegrationResponse)

-- | The configuration for what data should be pulled from the source.
getDataIntegrationResponse_objectConfiguration :: Lens.Lens' GetDataIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text))))
getDataIntegrationResponse_objectConfiguration = Lens.lens (\GetDataIntegrationResponse' {objectConfiguration} -> objectConfiguration) (\s@GetDataIntegrationResponse' {} a -> s {objectConfiguration = a} :: GetDataIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the data and how often it should be pulled from the source.
getDataIntegrationResponse_scheduleConfiguration :: Lens.Lens' GetDataIntegrationResponse (Prelude.Maybe ScheduleConfiguration)
getDataIntegrationResponse_scheduleConfiguration = Lens.lens (\GetDataIntegrationResponse' {scheduleConfiguration} -> scheduleConfiguration) (\s@GetDataIntegrationResponse' {} a -> s {scheduleConfiguration = a} :: GetDataIntegrationResponse)

-- | The URI of the data source.
getDataIntegrationResponse_sourceURI :: Lens.Lens' GetDataIntegrationResponse (Prelude.Maybe Prelude.Text)
getDataIntegrationResponse_sourceURI = Lens.lens (\GetDataIntegrationResponse' {sourceURI} -> sourceURI) (\s@GetDataIntegrationResponse' {} a -> s {sourceURI = a} :: GetDataIntegrationResponse)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
getDataIntegrationResponse_tags :: Lens.Lens' GetDataIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDataIntegrationResponse_tags = Lens.lens (\GetDataIntegrationResponse' {tags} -> tags) (\s@GetDataIntegrationResponse' {} a -> s {tags = a} :: GetDataIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDataIntegrationResponse_httpStatus :: Lens.Lens' GetDataIntegrationResponse Prelude.Int
getDataIntegrationResponse_httpStatus = Lens.lens (\GetDataIntegrationResponse' {httpStatus} -> httpStatus) (\s@GetDataIntegrationResponse' {} a -> s {httpStatus = a} :: GetDataIntegrationResponse)

instance Prelude.NFData GetDataIntegrationResponse where
  rnf GetDataIntegrationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf fileConfiguration
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf objectConfiguration
      `Prelude.seq` Prelude.rnf scheduleConfiguration
      `Prelude.seq` Prelude.rnf sourceURI
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
