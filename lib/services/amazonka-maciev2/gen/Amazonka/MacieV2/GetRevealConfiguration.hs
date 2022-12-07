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
-- Module      : Amazonka.MacieV2.GetRevealConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status and configuration settings for retrieving
-- occurrences of sensitive data reported by findings.
module Amazonka.MacieV2.GetRevealConfiguration
  ( -- * Creating a Request
    GetRevealConfiguration (..),
    newGetRevealConfiguration,

    -- * Destructuring the Response
    GetRevealConfigurationResponse (..),
    newGetRevealConfigurationResponse,

    -- * Response Lenses
    getRevealConfigurationResponse_configuration,
    getRevealConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRevealConfiguration' smart constructor.
data GetRevealConfiguration = GetRevealConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRevealConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetRevealConfiguration ::
  GetRevealConfiguration
newGetRevealConfiguration = GetRevealConfiguration'

instance Core.AWSRequest GetRevealConfiguration where
  type
    AWSResponse GetRevealConfiguration =
      GetRevealConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRevealConfigurationResponse'
            Prelude.<$> (x Data..?> "configuration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRevealConfiguration where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetRevealConfiguration where
  rnf _ = ()

instance Data.ToHeaders GetRevealConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRevealConfiguration where
  toPath = Prelude.const "/reveal-configuration"

instance Data.ToQuery GetRevealConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRevealConfigurationResponse' smart constructor.
data GetRevealConfigurationResponse = GetRevealConfigurationResponse'
  { -- | The current configuration settings and the status of the configuration
    -- for the account.
    configuration :: Prelude.Maybe RevealConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRevealConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'getRevealConfigurationResponse_configuration' - The current configuration settings and the status of the configuration
-- for the account.
--
-- 'httpStatus', 'getRevealConfigurationResponse_httpStatus' - The response's http status code.
newGetRevealConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRevealConfigurationResponse
newGetRevealConfigurationResponse pHttpStatus_ =
  GetRevealConfigurationResponse'
    { configuration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current configuration settings and the status of the configuration
-- for the account.
getRevealConfigurationResponse_configuration :: Lens.Lens' GetRevealConfigurationResponse (Prelude.Maybe RevealConfiguration)
getRevealConfigurationResponse_configuration = Lens.lens (\GetRevealConfigurationResponse' {configuration} -> configuration) (\s@GetRevealConfigurationResponse' {} a -> s {configuration = a} :: GetRevealConfigurationResponse)

-- | The response's http status code.
getRevealConfigurationResponse_httpStatus :: Lens.Lens' GetRevealConfigurationResponse Prelude.Int
getRevealConfigurationResponse_httpStatus = Lens.lens (\GetRevealConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetRevealConfigurationResponse' {} a -> s {httpStatus = a} :: GetRevealConfigurationResponse)

instance
  Prelude.NFData
    GetRevealConfigurationResponse
  where
  rnf GetRevealConfigurationResponse' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf httpStatus
