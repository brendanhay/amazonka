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
-- Module      : Amazonka.CloudHSM.GetConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <https://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Gets the configuration files necessary to connect to all high
-- availability partition groups the client is associated with.
module Amazonka.CloudHSM.GetConfig
  ( -- * Creating a Request
    GetConfig (..),
    newGetConfig,

    -- * Request Lenses
    getConfig_clientArn,
    getConfig_clientVersion,
    getConfig_hapgList,

    -- * Destructuring the Response
    GetConfigResponse (..),
    newGetConfigResponse,

    -- * Response Lenses
    getConfigResponse_configCred,
    getConfigResponse_configFile,
    getConfigResponse_configType,
    getConfigResponse_httpStatus,
  )
where

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConfig' smart constructor.
data GetConfig = GetConfig'
  { -- | The ARN of the client.
    clientArn :: Prelude.Text,
    -- | The client version.
    clientVersion :: ClientVersion,
    -- | A list of ARNs that identify the high-availability partition groups that
    -- are associated with the client.
    hapgList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientArn', 'getConfig_clientArn' - The ARN of the client.
--
-- 'clientVersion', 'getConfig_clientVersion' - The client version.
--
-- 'hapgList', 'getConfig_hapgList' - A list of ARNs that identify the high-availability partition groups that
-- are associated with the client.
newGetConfig ::
  -- | 'clientArn'
  Prelude.Text ->
  -- | 'clientVersion'
  ClientVersion ->
  GetConfig
newGetConfig pClientArn_ pClientVersion_ =
  GetConfig'
    { clientArn = pClientArn_,
      clientVersion = pClientVersion_,
      hapgList = Prelude.mempty
    }

-- | The ARN of the client.
getConfig_clientArn :: Lens.Lens' GetConfig Prelude.Text
getConfig_clientArn = Lens.lens (\GetConfig' {clientArn} -> clientArn) (\s@GetConfig' {} a -> s {clientArn = a} :: GetConfig)

-- | The client version.
getConfig_clientVersion :: Lens.Lens' GetConfig ClientVersion
getConfig_clientVersion = Lens.lens (\GetConfig' {clientVersion} -> clientVersion) (\s@GetConfig' {} a -> s {clientVersion = a} :: GetConfig)

-- | A list of ARNs that identify the high-availability partition groups that
-- are associated with the client.
getConfig_hapgList :: Lens.Lens' GetConfig [Prelude.Text]
getConfig_hapgList = Lens.lens (\GetConfig' {hapgList} -> hapgList) (\s@GetConfig' {} a -> s {hapgList = a} :: GetConfig) Prelude.. Lens.coerced

instance Core.AWSRequest GetConfig where
  type AWSResponse GetConfig = GetConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfigResponse'
            Prelude.<$> (x Data..?> "ConfigCred")
            Prelude.<*> (x Data..?> "ConfigFile")
            Prelude.<*> (x Data..?> "ConfigType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConfig where
  hashWithSalt _salt GetConfig' {..} =
    _salt
      `Prelude.hashWithSalt` clientArn
      `Prelude.hashWithSalt` clientVersion
      `Prelude.hashWithSalt` hapgList

instance Prelude.NFData GetConfig where
  rnf GetConfig' {..} =
    Prelude.rnf clientArn
      `Prelude.seq` Prelude.rnf clientVersion
      `Prelude.seq` Prelude.rnf hapgList

instance Data.ToHeaders GetConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.GetConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetConfig where
  toJSON GetConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClientArn" Data..= clientArn),
            Prelude.Just ("ClientVersion" Data..= clientVersion),
            Prelude.Just ("HapgList" Data..= hapgList)
          ]
      )

instance Data.ToPath GetConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery GetConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConfigResponse' smart constructor.
data GetConfigResponse = GetConfigResponse'
  { -- | The certificate file containing the server.pem files of the HSMs.
    configCred :: Prelude.Maybe Prelude.Text,
    -- | The chrystoki.conf configuration file.
    configFile :: Prelude.Maybe Prelude.Text,
    -- | The type of credentials.
    configType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configCred', 'getConfigResponse_configCred' - The certificate file containing the server.pem files of the HSMs.
--
-- 'configFile', 'getConfigResponse_configFile' - The chrystoki.conf configuration file.
--
-- 'configType', 'getConfigResponse_configType' - The type of credentials.
--
-- 'httpStatus', 'getConfigResponse_httpStatus' - The response's http status code.
newGetConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConfigResponse
newGetConfigResponse pHttpStatus_ =
  GetConfigResponse'
    { configCred = Prelude.Nothing,
      configFile = Prelude.Nothing,
      configType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The certificate file containing the server.pem files of the HSMs.
getConfigResponse_configCred :: Lens.Lens' GetConfigResponse (Prelude.Maybe Prelude.Text)
getConfigResponse_configCred = Lens.lens (\GetConfigResponse' {configCred} -> configCred) (\s@GetConfigResponse' {} a -> s {configCred = a} :: GetConfigResponse)

-- | The chrystoki.conf configuration file.
getConfigResponse_configFile :: Lens.Lens' GetConfigResponse (Prelude.Maybe Prelude.Text)
getConfigResponse_configFile = Lens.lens (\GetConfigResponse' {configFile} -> configFile) (\s@GetConfigResponse' {} a -> s {configFile = a} :: GetConfigResponse)

-- | The type of credentials.
getConfigResponse_configType :: Lens.Lens' GetConfigResponse (Prelude.Maybe Prelude.Text)
getConfigResponse_configType = Lens.lens (\GetConfigResponse' {configType} -> configType) (\s@GetConfigResponse' {} a -> s {configType = a} :: GetConfigResponse)

-- | The response's http status code.
getConfigResponse_httpStatus :: Lens.Lens' GetConfigResponse Prelude.Int
getConfigResponse_httpStatus = Lens.lens (\GetConfigResponse' {httpStatus} -> httpStatus) (\s@GetConfigResponse' {} a -> s {httpStatus = a} :: GetConfigResponse)

instance Prelude.NFData GetConfigResponse where
  rnf GetConfigResponse' {..} =
    Prelude.rnf configCred
      `Prelude.seq` Prelude.rnf configFile
      `Prelude.seq` Prelude.rnf configType
      `Prelude.seq` Prelude.rnf httpStatus
