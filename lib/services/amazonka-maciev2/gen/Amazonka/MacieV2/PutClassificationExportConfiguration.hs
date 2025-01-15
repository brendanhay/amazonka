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
-- Module      : Amazonka.MacieV2.PutClassificationExportConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the configuration settings for storing data
-- classification results.
module Amazonka.MacieV2.PutClassificationExportConfiguration
  ( -- * Creating a Request
    PutClassificationExportConfiguration (..),
    newPutClassificationExportConfiguration,

    -- * Request Lenses
    putClassificationExportConfiguration_configuration,

    -- * Destructuring the Response
    PutClassificationExportConfigurationResponse (..),
    newPutClassificationExportConfigurationResponse,

    -- * Response Lenses
    putClassificationExportConfigurationResponse_configuration,
    putClassificationExportConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutClassificationExportConfiguration' smart constructor.
data PutClassificationExportConfiguration = PutClassificationExportConfiguration'
  { -- | The location to store data classification results in, and the encryption
    -- settings to use when storing results in that location.
    configuration :: ClassificationExportConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutClassificationExportConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'putClassificationExportConfiguration_configuration' - The location to store data classification results in, and the encryption
-- settings to use when storing results in that location.
newPutClassificationExportConfiguration ::
  -- | 'configuration'
  ClassificationExportConfiguration ->
  PutClassificationExportConfiguration
newPutClassificationExportConfiguration
  pConfiguration_ =
    PutClassificationExportConfiguration'
      { configuration =
          pConfiguration_
      }

-- | The location to store data classification results in, and the encryption
-- settings to use when storing results in that location.
putClassificationExportConfiguration_configuration :: Lens.Lens' PutClassificationExportConfiguration ClassificationExportConfiguration
putClassificationExportConfiguration_configuration = Lens.lens (\PutClassificationExportConfiguration' {configuration} -> configuration) (\s@PutClassificationExportConfiguration' {} a -> s {configuration = a} :: PutClassificationExportConfiguration)

instance
  Core.AWSRequest
    PutClassificationExportConfiguration
  where
  type
    AWSResponse PutClassificationExportConfiguration =
      PutClassificationExportConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutClassificationExportConfigurationResponse'
            Prelude.<$> (x Data..?> "configuration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutClassificationExportConfiguration
  where
  hashWithSalt
    _salt
    PutClassificationExportConfiguration' {..} =
      _salt `Prelude.hashWithSalt` configuration

instance
  Prelude.NFData
    PutClassificationExportConfiguration
  where
  rnf PutClassificationExportConfiguration' {..} =
    Prelude.rnf configuration

instance
  Data.ToHeaders
    PutClassificationExportConfiguration
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

instance
  Data.ToJSON
    PutClassificationExportConfiguration
  where
  toJSON PutClassificationExportConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("configuration" Data..= configuration)
          ]
      )

instance
  Data.ToPath
    PutClassificationExportConfiguration
  where
  toPath =
    Prelude.const
      "/classification-export-configuration"

instance
  Data.ToQuery
    PutClassificationExportConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutClassificationExportConfigurationResponse' smart constructor.
data PutClassificationExportConfigurationResponse = PutClassificationExportConfigurationResponse'
  { -- | The location where the data classification results are stored, and the
    -- encryption settings that are used when storing results in that location.
    configuration :: Prelude.Maybe ClassificationExportConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutClassificationExportConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'putClassificationExportConfigurationResponse_configuration' - The location where the data classification results are stored, and the
-- encryption settings that are used when storing results in that location.
--
-- 'httpStatus', 'putClassificationExportConfigurationResponse_httpStatus' - The response's http status code.
newPutClassificationExportConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutClassificationExportConfigurationResponse
newPutClassificationExportConfigurationResponse
  pHttpStatus_ =
    PutClassificationExportConfigurationResponse'
      { configuration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The location where the data classification results are stored, and the
-- encryption settings that are used when storing results in that location.
putClassificationExportConfigurationResponse_configuration :: Lens.Lens' PutClassificationExportConfigurationResponse (Prelude.Maybe ClassificationExportConfiguration)
putClassificationExportConfigurationResponse_configuration = Lens.lens (\PutClassificationExportConfigurationResponse' {configuration} -> configuration) (\s@PutClassificationExportConfigurationResponse' {} a -> s {configuration = a} :: PutClassificationExportConfigurationResponse)

-- | The response's http status code.
putClassificationExportConfigurationResponse_httpStatus :: Lens.Lens' PutClassificationExportConfigurationResponse Prelude.Int
putClassificationExportConfigurationResponse_httpStatus = Lens.lens (\PutClassificationExportConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutClassificationExportConfigurationResponse' {} a -> s {httpStatus = a} :: PutClassificationExportConfigurationResponse)

instance
  Prelude.NFData
    PutClassificationExportConfigurationResponse
  where
  rnf PutClassificationExportConfigurationResponse' {..} =
    Prelude.rnf configuration `Prelude.seq`
      Prelude.rnf httpStatus
