{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudHSM.GetConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Gets the configuration files necessary to connect to all high
-- availability partition groups the client is associated with.
module Network.AWS.CloudHSM.GetConfig
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
    getConfigResponse_configFile,
    getConfigResponse_configCred,
    getConfigResponse_configType,
    getConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
getConfig_hapgList = Lens.lens (\GetConfig' {hapgList} -> hapgList) (\s@GetConfig' {} a -> s {hapgList = a} :: GetConfig) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest GetConfig where
  type Rs GetConfig = GetConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfigResponse'
            Prelude.<$> (x Prelude..?> "ConfigFile")
            Prelude.<*> (x Prelude..?> "ConfigCred")
            Prelude.<*> (x Prelude..?> "ConfigType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConfig

instance Prelude.NFData GetConfig

instance Prelude.ToHeaders GetConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CloudHsmFrontendService.GetConfig" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetConfig where
  toJSON GetConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClientArn" Prelude..= clientArn),
            Prelude.Just
              ("ClientVersion" Prelude..= clientVersion),
            Prelude.Just ("HapgList" Prelude..= hapgList)
          ]
      )

instance Prelude.ToPath GetConfig where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConfigResponse' smart constructor.
data GetConfigResponse = GetConfigResponse'
  { -- | The chrystoki.conf configuration file.
    configFile :: Prelude.Maybe Prelude.Text,
    -- | The certificate file containing the server.pem files of the HSMs.
    configCred :: Prelude.Maybe Prelude.Text,
    -- | The type of credentials.
    configType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configFile', 'getConfigResponse_configFile' - The chrystoki.conf configuration file.
--
-- 'configCred', 'getConfigResponse_configCred' - The certificate file containing the server.pem files of the HSMs.
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
    { configFile = Prelude.Nothing,
      configCred = Prelude.Nothing,
      configType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The chrystoki.conf configuration file.
getConfigResponse_configFile :: Lens.Lens' GetConfigResponse (Prelude.Maybe Prelude.Text)
getConfigResponse_configFile = Lens.lens (\GetConfigResponse' {configFile} -> configFile) (\s@GetConfigResponse' {} a -> s {configFile = a} :: GetConfigResponse)

-- | The certificate file containing the server.pem files of the HSMs.
getConfigResponse_configCred :: Lens.Lens' GetConfigResponse (Prelude.Maybe Prelude.Text)
getConfigResponse_configCred = Lens.lens (\GetConfigResponse' {configCred} -> configCred) (\s@GetConfigResponse' {} a -> s {configCred = a} :: GetConfigResponse)

-- | The type of credentials.
getConfigResponse_configType :: Lens.Lens' GetConfigResponse (Prelude.Maybe Prelude.Text)
getConfigResponse_configType = Lens.lens (\GetConfigResponse' {configType} -> configType) (\s@GetConfigResponse' {} a -> s {configType = a} :: GetConfigResponse)

-- | The response's http status code.
getConfigResponse_httpStatus :: Lens.Lens' GetConfigResponse Prelude.Int
getConfigResponse_httpStatus = Lens.lens (\GetConfigResponse' {httpStatus} -> httpStatus) (\s@GetConfigResponse' {} a -> s {httpStatus = a} :: GetConfigResponse)

instance Prelude.NFData GetConfigResponse
