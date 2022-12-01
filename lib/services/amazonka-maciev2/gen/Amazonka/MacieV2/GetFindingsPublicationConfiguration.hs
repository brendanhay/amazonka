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
-- Module      : Amazonka.MacieV2.GetFindingsPublicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration settings for publishing findings to Security
-- Hub.
module Amazonka.MacieV2.GetFindingsPublicationConfiguration
  ( -- * Creating a Request
    GetFindingsPublicationConfiguration (..),
    newGetFindingsPublicationConfiguration,

    -- * Destructuring the Response
    GetFindingsPublicationConfigurationResponse (..),
    newGetFindingsPublicationConfigurationResponse,

    -- * Response Lenses
    getFindingsPublicationConfigurationResponse_securityHubConfiguration,
    getFindingsPublicationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFindingsPublicationConfiguration' smart constructor.
data GetFindingsPublicationConfiguration = GetFindingsPublicationConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingsPublicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetFindingsPublicationConfiguration ::
  GetFindingsPublicationConfiguration
newGetFindingsPublicationConfiguration =
  GetFindingsPublicationConfiguration'

instance
  Core.AWSRequest
    GetFindingsPublicationConfiguration
  where
  type
    AWSResponse GetFindingsPublicationConfiguration =
      GetFindingsPublicationConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFindingsPublicationConfigurationResponse'
            Prelude.<$> (x Core..?> "securityHubConfiguration")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetFindingsPublicationConfiguration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetFindingsPublicationConfiguration
  where
  rnf _ = ()

instance
  Core.ToHeaders
    GetFindingsPublicationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToPath
    GetFindingsPublicationConfiguration
  where
  toPath =
    Prelude.const "/findings-publication-configuration"

instance
  Core.ToQuery
    GetFindingsPublicationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFindingsPublicationConfigurationResponse' smart constructor.
data GetFindingsPublicationConfigurationResponse = GetFindingsPublicationConfigurationResponse'
  { -- | The configuration settings that determine which findings are published
    -- to Security Hub.
    securityHubConfiguration :: Prelude.Maybe SecurityHubConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingsPublicationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityHubConfiguration', 'getFindingsPublicationConfigurationResponse_securityHubConfiguration' - The configuration settings that determine which findings are published
-- to Security Hub.
--
-- 'httpStatus', 'getFindingsPublicationConfigurationResponse_httpStatus' - The response's http status code.
newGetFindingsPublicationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFindingsPublicationConfigurationResponse
newGetFindingsPublicationConfigurationResponse
  pHttpStatus_ =
    GetFindingsPublicationConfigurationResponse'
      { securityHubConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The configuration settings that determine which findings are published
-- to Security Hub.
getFindingsPublicationConfigurationResponse_securityHubConfiguration :: Lens.Lens' GetFindingsPublicationConfigurationResponse (Prelude.Maybe SecurityHubConfiguration)
getFindingsPublicationConfigurationResponse_securityHubConfiguration = Lens.lens (\GetFindingsPublicationConfigurationResponse' {securityHubConfiguration} -> securityHubConfiguration) (\s@GetFindingsPublicationConfigurationResponse' {} a -> s {securityHubConfiguration = a} :: GetFindingsPublicationConfigurationResponse)

-- | The response's http status code.
getFindingsPublicationConfigurationResponse_httpStatus :: Lens.Lens' GetFindingsPublicationConfigurationResponse Prelude.Int
getFindingsPublicationConfigurationResponse_httpStatus = Lens.lens (\GetFindingsPublicationConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetFindingsPublicationConfigurationResponse' {} a -> s {httpStatus = a} :: GetFindingsPublicationConfigurationResponse)

instance
  Prelude.NFData
    GetFindingsPublicationConfigurationResponse
  where
  rnf GetFindingsPublicationConfigurationResponse' {..} =
    Prelude.rnf securityHubConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
