{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoIdentityProvider.Types.ContextDataType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.ContextDataType where

import Amazonka.CognitoIdentityProvider.Types.HttpHeader
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contextual user data type used for evaluating the risk of an unexpected
-- event by Amazon Cognito advanced security.
--
-- /See:/ 'newContextDataType' smart constructor.
data ContextDataType = ContextDataType'
  { -- | Encoded device-fingerprint details that your app collected with the
    -- Amazon Cognito context data collection library. For more information,
    -- see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-adaptive-authentication.html#user-pool-settings-adaptive-authentication-device-fingerprint Adding user device and session data to API requests>.
    encodedData :: Prelude.Maybe Prelude.Text,
    -- | The source IP address of your user\'s device.
    ipAddress :: Prelude.Text,
    -- | Your server endpoint where this API is invoked.
    serverName :: Prelude.Text,
    -- | Your server path where this API is invoked.
    serverPath :: Prelude.Text,
    -- | HttpHeaders received on your server in same order.
    httpHeaders :: [HttpHeader]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContextDataType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encodedData', 'contextDataType_encodedData' - Encoded device-fingerprint details that your app collected with the
-- Amazon Cognito context data collection library. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-adaptive-authentication.html#user-pool-settings-adaptive-authentication-device-fingerprint Adding user device and session data to API requests>.
--
-- 'ipAddress', 'contextDataType_ipAddress' - The source IP address of your user\'s device.
--
-- 'serverName', 'contextDataType_serverName' - Your server endpoint where this API is invoked.
--
-- 'serverPath', 'contextDataType_serverPath' - Your server path where this API is invoked.
--
-- 'httpHeaders', 'contextDataType_httpHeaders' - HttpHeaders received on your server in same order.
newContextDataType ::
  -- | 'ipAddress'
  Prelude.Text ->
  -- | 'serverName'
  Prelude.Text ->
  -- | 'serverPath'
  Prelude.Text ->
  ContextDataType
newContextDataType
  pIpAddress_
  pServerName_
  pServerPath_ =
    ContextDataType'
      { encodedData = Prelude.Nothing,
        ipAddress = pIpAddress_,
        serverName = pServerName_,
        serverPath = pServerPath_,
        httpHeaders = Prelude.mempty
      }

-- | Encoded device-fingerprint details that your app collected with the
-- Amazon Cognito context data collection library. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-adaptive-authentication.html#user-pool-settings-adaptive-authentication-device-fingerprint Adding user device and session data to API requests>.
contextDataType_encodedData :: Lens.Lens' ContextDataType (Prelude.Maybe Prelude.Text)
contextDataType_encodedData = Lens.lens (\ContextDataType' {encodedData} -> encodedData) (\s@ContextDataType' {} a -> s {encodedData = a} :: ContextDataType)

-- | The source IP address of your user\'s device.
contextDataType_ipAddress :: Lens.Lens' ContextDataType Prelude.Text
contextDataType_ipAddress = Lens.lens (\ContextDataType' {ipAddress} -> ipAddress) (\s@ContextDataType' {} a -> s {ipAddress = a} :: ContextDataType)

-- | Your server endpoint where this API is invoked.
contextDataType_serverName :: Lens.Lens' ContextDataType Prelude.Text
contextDataType_serverName = Lens.lens (\ContextDataType' {serverName} -> serverName) (\s@ContextDataType' {} a -> s {serverName = a} :: ContextDataType)

-- | Your server path where this API is invoked.
contextDataType_serverPath :: Lens.Lens' ContextDataType Prelude.Text
contextDataType_serverPath = Lens.lens (\ContextDataType' {serverPath} -> serverPath) (\s@ContextDataType' {} a -> s {serverPath = a} :: ContextDataType)

-- | HttpHeaders received on your server in same order.
contextDataType_httpHeaders :: Lens.Lens' ContextDataType [HttpHeader]
contextDataType_httpHeaders = Lens.lens (\ContextDataType' {httpHeaders} -> httpHeaders) (\s@ContextDataType' {} a -> s {httpHeaders = a} :: ContextDataType) Prelude.. Lens.coerced

instance Prelude.Hashable ContextDataType where
  hashWithSalt _salt ContextDataType' {..} =
    _salt `Prelude.hashWithSalt` encodedData
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` serverPath
      `Prelude.hashWithSalt` httpHeaders

instance Prelude.NFData ContextDataType where
  rnf ContextDataType' {..} =
    Prelude.rnf encodedData
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf serverPath
      `Prelude.seq` Prelude.rnf httpHeaders

instance Data.ToJSON ContextDataType where
  toJSON ContextDataType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncodedData" Data..=) Prelude.<$> encodedData,
            Prelude.Just ("IpAddress" Data..= ipAddress),
            Prelude.Just ("ServerName" Data..= serverName),
            Prelude.Just ("ServerPath" Data..= serverPath),
            Prelude.Just ("HttpHeaders" Data..= httpHeaders)
          ]
      )
