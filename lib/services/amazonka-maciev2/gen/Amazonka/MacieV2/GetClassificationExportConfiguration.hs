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
-- Module      : Amazonka.MacieV2.GetClassificationExportConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration settings for storing data classification
-- results.
module Amazonka.MacieV2.GetClassificationExportConfiguration
  ( -- * Creating a Request
    GetClassificationExportConfiguration (..),
    newGetClassificationExportConfiguration,

    -- * Destructuring the Response
    GetClassificationExportConfigurationResponse (..),
    newGetClassificationExportConfigurationResponse,

    -- * Response Lenses
    getClassificationExportConfigurationResponse_configuration,
    getClassificationExportConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetClassificationExportConfiguration' smart constructor.
data GetClassificationExportConfiguration = GetClassificationExportConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClassificationExportConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetClassificationExportConfiguration ::
  GetClassificationExportConfiguration
newGetClassificationExportConfiguration =
  GetClassificationExportConfiguration'

instance
  Core.AWSRequest
    GetClassificationExportConfiguration
  where
  type
    AWSResponse GetClassificationExportConfiguration =
      GetClassificationExportConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetClassificationExportConfigurationResponse'
            Prelude.<$> (x Data..?> "configuration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetClassificationExportConfiguration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetClassificationExportConfiguration
  where
  rnf _ = ()

instance
  Data.ToHeaders
    GetClassificationExportConfiguration
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
  Data.ToPath
    GetClassificationExportConfiguration
  where
  toPath =
    Prelude.const
      "/classification-export-configuration"

instance
  Data.ToQuery
    GetClassificationExportConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetClassificationExportConfigurationResponse' smart constructor.
data GetClassificationExportConfigurationResponse = GetClassificationExportConfigurationResponse'
  { -- | The location where data classification results are stored, and the
    -- encryption settings that are used when storing results in that location.
    configuration :: Prelude.Maybe ClassificationExportConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClassificationExportConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'getClassificationExportConfigurationResponse_configuration' - The location where data classification results are stored, and the
-- encryption settings that are used when storing results in that location.
--
-- 'httpStatus', 'getClassificationExportConfigurationResponse_httpStatus' - The response's http status code.
newGetClassificationExportConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetClassificationExportConfigurationResponse
newGetClassificationExportConfigurationResponse
  pHttpStatus_ =
    GetClassificationExportConfigurationResponse'
      { configuration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The location where data classification results are stored, and the
-- encryption settings that are used when storing results in that location.
getClassificationExportConfigurationResponse_configuration :: Lens.Lens' GetClassificationExportConfigurationResponse (Prelude.Maybe ClassificationExportConfiguration)
getClassificationExportConfigurationResponse_configuration = Lens.lens (\GetClassificationExportConfigurationResponse' {configuration} -> configuration) (\s@GetClassificationExportConfigurationResponse' {} a -> s {configuration = a} :: GetClassificationExportConfigurationResponse)

-- | The response's http status code.
getClassificationExportConfigurationResponse_httpStatus :: Lens.Lens' GetClassificationExportConfigurationResponse Prelude.Int
getClassificationExportConfigurationResponse_httpStatus = Lens.lens (\GetClassificationExportConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetClassificationExportConfigurationResponse' {} a -> s {httpStatus = a} :: GetClassificationExportConfigurationResponse)

instance
  Prelude.NFData
    GetClassificationExportConfigurationResponse
  where
  rnf GetClassificationExportConfigurationResponse' {..} =
    Prelude.rnf configuration `Prelude.seq`
      Prelude.rnf httpStatus
