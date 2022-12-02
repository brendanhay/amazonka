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
-- Module      : Amazonka.MacieV2.PutFindingsPublicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration settings for publishing findings to Security
-- Hub.
module Amazonka.MacieV2.PutFindingsPublicationConfiguration
  ( -- * Creating a Request
    PutFindingsPublicationConfiguration (..),
    newPutFindingsPublicationConfiguration,

    -- * Request Lenses
    putFindingsPublicationConfiguration_clientToken,
    putFindingsPublicationConfiguration_securityHubConfiguration,

    -- * Destructuring the Response
    PutFindingsPublicationConfigurationResponse (..),
    newPutFindingsPublicationConfigurationResponse,

    -- * Response Lenses
    putFindingsPublicationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutFindingsPublicationConfiguration' smart constructor.
data PutFindingsPublicationConfiguration = PutFindingsPublicationConfiguration'
  { -- | A unique, case-sensitive token that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The configuration settings that determine which findings to publish to
    -- Security Hub.
    securityHubConfiguration :: Prelude.Maybe SecurityHubConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutFindingsPublicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'putFindingsPublicationConfiguration_clientToken' - A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
--
-- 'securityHubConfiguration', 'putFindingsPublicationConfiguration_securityHubConfiguration' - The configuration settings that determine which findings to publish to
-- Security Hub.
newPutFindingsPublicationConfiguration ::
  PutFindingsPublicationConfiguration
newPutFindingsPublicationConfiguration =
  PutFindingsPublicationConfiguration'
    { clientToken =
        Prelude.Nothing,
      securityHubConfiguration =
        Prelude.Nothing
    }

-- | A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
putFindingsPublicationConfiguration_clientToken :: Lens.Lens' PutFindingsPublicationConfiguration (Prelude.Maybe Prelude.Text)
putFindingsPublicationConfiguration_clientToken = Lens.lens (\PutFindingsPublicationConfiguration' {clientToken} -> clientToken) (\s@PutFindingsPublicationConfiguration' {} a -> s {clientToken = a} :: PutFindingsPublicationConfiguration)

-- | The configuration settings that determine which findings to publish to
-- Security Hub.
putFindingsPublicationConfiguration_securityHubConfiguration :: Lens.Lens' PutFindingsPublicationConfiguration (Prelude.Maybe SecurityHubConfiguration)
putFindingsPublicationConfiguration_securityHubConfiguration = Lens.lens (\PutFindingsPublicationConfiguration' {securityHubConfiguration} -> securityHubConfiguration) (\s@PutFindingsPublicationConfiguration' {} a -> s {securityHubConfiguration = a} :: PutFindingsPublicationConfiguration)

instance
  Core.AWSRequest
    PutFindingsPublicationConfiguration
  where
  type
    AWSResponse PutFindingsPublicationConfiguration =
      PutFindingsPublicationConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutFindingsPublicationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutFindingsPublicationConfiguration
  where
  hashWithSalt
    _salt
    PutFindingsPublicationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` securityHubConfiguration

instance
  Prelude.NFData
    PutFindingsPublicationConfiguration
  where
  rnf PutFindingsPublicationConfiguration' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf securityHubConfiguration

instance
  Data.ToHeaders
    PutFindingsPublicationConfiguration
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
    PutFindingsPublicationConfiguration
  where
  toJSON PutFindingsPublicationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("securityHubConfiguration" Data..=)
              Prelude.<$> securityHubConfiguration
          ]
      )

instance
  Data.ToPath
    PutFindingsPublicationConfiguration
  where
  toPath =
    Prelude.const "/findings-publication-configuration"

instance
  Data.ToQuery
    PutFindingsPublicationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutFindingsPublicationConfigurationResponse' smart constructor.
data PutFindingsPublicationConfigurationResponse = PutFindingsPublicationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutFindingsPublicationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putFindingsPublicationConfigurationResponse_httpStatus' - The response's http status code.
newPutFindingsPublicationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutFindingsPublicationConfigurationResponse
newPutFindingsPublicationConfigurationResponse
  pHttpStatus_ =
    PutFindingsPublicationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putFindingsPublicationConfigurationResponse_httpStatus :: Lens.Lens' PutFindingsPublicationConfigurationResponse Prelude.Int
putFindingsPublicationConfigurationResponse_httpStatus = Lens.lens (\PutFindingsPublicationConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutFindingsPublicationConfigurationResponse' {} a -> s {httpStatus = a} :: PutFindingsPublicationConfigurationResponse)

instance
  Prelude.NFData
    PutFindingsPublicationConfigurationResponse
  where
  rnf PutFindingsPublicationConfigurationResponse' {..} =
    Prelude.rnf httpStatus
