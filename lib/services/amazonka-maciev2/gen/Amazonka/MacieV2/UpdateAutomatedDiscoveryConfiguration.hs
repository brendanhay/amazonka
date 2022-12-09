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
-- Module      : Amazonka.MacieV2.UpdateAutomatedDiscoveryConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables automated sensitive data discovery for an account.
module Amazonka.MacieV2.UpdateAutomatedDiscoveryConfiguration
  ( -- * Creating a Request
    UpdateAutomatedDiscoveryConfiguration (..),
    newUpdateAutomatedDiscoveryConfiguration,

    -- * Request Lenses
    updateAutomatedDiscoveryConfiguration_status,

    -- * Destructuring the Response
    UpdateAutomatedDiscoveryConfigurationResponse (..),
    newUpdateAutomatedDiscoveryConfigurationResponse,

    -- * Response Lenses
    updateAutomatedDiscoveryConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAutomatedDiscoveryConfiguration' smart constructor.
data UpdateAutomatedDiscoveryConfiguration = UpdateAutomatedDiscoveryConfiguration'
  { -- | The new status of automated sensitive data discovery for the account.
    -- Valid values are: ENABLED, start or resume automated sensitive data
    -- discovery activities for the account; and, DISABLED, stop performing
    -- automated sensitive data discovery activities for the account.
    --
    -- When you enable automated sensitive data discovery for the first time,
    -- Amazon Macie uses default configuration settings to determine which data
    -- sources to analyze and which managed data identifiers to use. To change
    -- these settings, use the UpdateClassificationScope and
    -- UpdateSensitivityInspectionTemplate operations, respectively. If you
    -- change the settings and subsequently disable the configuration, Amazon
    -- Macie retains your changes.
    status :: AutomatedDiscoveryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAutomatedDiscoveryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updateAutomatedDiscoveryConfiguration_status' - The new status of automated sensitive data discovery for the account.
-- Valid values are: ENABLED, start or resume automated sensitive data
-- discovery activities for the account; and, DISABLED, stop performing
-- automated sensitive data discovery activities for the account.
--
-- When you enable automated sensitive data discovery for the first time,
-- Amazon Macie uses default configuration settings to determine which data
-- sources to analyze and which managed data identifiers to use. To change
-- these settings, use the UpdateClassificationScope and
-- UpdateSensitivityInspectionTemplate operations, respectively. If you
-- change the settings and subsequently disable the configuration, Amazon
-- Macie retains your changes.
newUpdateAutomatedDiscoveryConfiguration ::
  -- | 'status'
  AutomatedDiscoveryStatus ->
  UpdateAutomatedDiscoveryConfiguration
newUpdateAutomatedDiscoveryConfiguration pStatus_ =
  UpdateAutomatedDiscoveryConfiguration'
    { status =
        pStatus_
    }

-- | The new status of automated sensitive data discovery for the account.
-- Valid values are: ENABLED, start or resume automated sensitive data
-- discovery activities for the account; and, DISABLED, stop performing
-- automated sensitive data discovery activities for the account.
--
-- When you enable automated sensitive data discovery for the first time,
-- Amazon Macie uses default configuration settings to determine which data
-- sources to analyze and which managed data identifiers to use. To change
-- these settings, use the UpdateClassificationScope and
-- UpdateSensitivityInspectionTemplate operations, respectively. If you
-- change the settings and subsequently disable the configuration, Amazon
-- Macie retains your changes.
updateAutomatedDiscoveryConfiguration_status :: Lens.Lens' UpdateAutomatedDiscoveryConfiguration AutomatedDiscoveryStatus
updateAutomatedDiscoveryConfiguration_status = Lens.lens (\UpdateAutomatedDiscoveryConfiguration' {status} -> status) (\s@UpdateAutomatedDiscoveryConfiguration' {} a -> s {status = a} :: UpdateAutomatedDiscoveryConfiguration)

instance
  Core.AWSRequest
    UpdateAutomatedDiscoveryConfiguration
  where
  type
    AWSResponse
      UpdateAutomatedDiscoveryConfiguration =
      UpdateAutomatedDiscoveryConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAutomatedDiscoveryConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateAutomatedDiscoveryConfiguration
  where
  hashWithSalt
    _salt
    UpdateAutomatedDiscoveryConfiguration' {..} =
      _salt `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    UpdateAutomatedDiscoveryConfiguration
  where
  rnf UpdateAutomatedDiscoveryConfiguration' {..} =
    Prelude.rnf status

instance
  Data.ToHeaders
    UpdateAutomatedDiscoveryConfiguration
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
    UpdateAutomatedDiscoveryConfiguration
  where
  toJSON UpdateAutomatedDiscoveryConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("status" Data..= status)]
      )

instance
  Data.ToPath
    UpdateAutomatedDiscoveryConfiguration
  where
  toPath =
    Prelude.const "/automated-discovery/configuration"

instance
  Data.ToQuery
    UpdateAutomatedDiscoveryConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAutomatedDiscoveryConfigurationResponse' smart constructor.
data UpdateAutomatedDiscoveryConfigurationResponse = UpdateAutomatedDiscoveryConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAutomatedDiscoveryConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAutomatedDiscoveryConfigurationResponse_httpStatus' - The response's http status code.
newUpdateAutomatedDiscoveryConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAutomatedDiscoveryConfigurationResponse
newUpdateAutomatedDiscoveryConfigurationResponse
  pHttpStatus_ =
    UpdateAutomatedDiscoveryConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateAutomatedDiscoveryConfigurationResponse_httpStatus :: Lens.Lens' UpdateAutomatedDiscoveryConfigurationResponse Prelude.Int
updateAutomatedDiscoveryConfigurationResponse_httpStatus = Lens.lens (\UpdateAutomatedDiscoveryConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateAutomatedDiscoveryConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateAutomatedDiscoveryConfigurationResponse)

instance
  Prelude.NFData
    UpdateAutomatedDiscoveryConfigurationResponse
  where
  rnf
    UpdateAutomatedDiscoveryConfigurationResponse' {..} =
      Prelude.rnf httpStatus
