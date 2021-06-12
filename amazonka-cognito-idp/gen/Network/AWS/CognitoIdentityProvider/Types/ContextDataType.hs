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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ContextDataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ContextDataType where

import Network.AWS.CognitoIdentityProvider.Types.HttpHeader
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contextual user data type used for evaluating the risk of an unexpected
-- event by Amazon Cognito advanced security.
--
-- /See:/ 'newContextDataType' smart constructor.
data ContextDataType = ContextDataType'
  { -- | Encoded data containing device fingerprinting details, collected using
    -- the Amazon Cognito context data collection library.
    encodedData :: Core.Maybe Core.Text,
    -- | Source IP address of your user.
    ipAddress :: Core.Text,
    -- | Your server endpoint where this API is invoked.
    serverName :: Core.Text,
    -- | Your server path where this API is invoked.
    serverPath :: Core.Text,
    -- | HttpHeaders received on your server in same order.
    httpHeaders :: [HttpHeader]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContextDataType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encodedData', 'contextDataType_encodedData' - Encoded data containing device fingerprinting details, collected using
-- the Amazon Cognito context data collection library.
--
-- 'ipAddress', 'contextDataType_ipAddress' - Source IP address of your user.
--
-- 'serverName', 'contextDataType_serverName' - Your server endpoint where this API is invoked.
--
-- 'serverPath', 'contextDataType_serverPath' - Your server path where this API is invoked.
--
-- 'httpHeaders', 'contextDataType_httpHeaders' - HttpHeaders received on your server in same order.
newContextDataType ::
  -- | 'ipAddress'
  Core.Text ->
  -- | 'serverName'
  Core.Text ->
  -- | 'serverPath'
  Core.Text ->
  ContextDataType
newContextDataType
  pIpAddress_
  pServerName_
  pServerPath_ =
    ContextDataType'
      { encodedData = Core.Nothing,
        ipAddress = pIpAddress_,
        serverName = pServerName_,
        serverPath = pServerPath_,
        httpHeaders = Core.mempty
      }

-- | Encoded data containing device fingerprinting details, collected using
-- the Amazon Cognito context data collection library.
contextDataType_encodedData :: Lens.Lens' ContextDataType (Core.Maybe Core.Text)
contextDataType_encodedData = Lens.lens (\ContextDataType' {encodedData} -> encodedData) (\s@ContextDataType' {} a -> s {encodedData = a} :: ContextDataType)

-- | Source IP address of your user.
contextDataType_ipAddress :: Lens.Lens' ContextDataType Core.Text
contextDataType_ipAddress = Lens.lens (\ContextDataType' {ipAddress} -> ipAddress) (\s@ContextDataType' {} a -> s {ipAddress = a} :: ContextDataType)

-- | Your server endpoint where this API is invoked.
contextDataType_serverName :: Lens.Lens' ContextDataType Core.Text
contextDataType_serverName = Lens.lens (\ContextDataType' {serverName} -> serverName) (\s@ContextDataType' {} a -> s {serverName = a} :: ContextDataType)

-- | Your server path where this API is invoked.
contextDataType_serverPath :: Lens.Lens' ContextDataType Core.Text
contextDataType_serverPath = Lens.lens (\ContextDataType' {serverPath} -> serverPath) (\s@ContextDataType' {} a -> s {serverPath = a} :: ContextDataType)

-- | HttpHeaders received on your server in same order.
contextDataType_httpHeaders :: Lens.Lens' ContextDataType [HttpHeader]
contextDataType_httpHeaders = Lens.lens (\ContextDataType' {httpHeaders} -> httpHeaders) (\s@ContextDataType' {} a -> s {httpHeaders = a} :: ContextDataType) Core.. Lens._Coerce

instance Core.Hashable ContextDataType

instance Core.NFData ContextDataType

instance Core.ToJSON ContextDataType where
  toJSON ContextDataType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EncodedData" Core..=) Core.<$> encodedData,
            Core.Just ("IpAddress" Core..= ipAddress),
            Core.Just ("ServerName" Core..= serverName),
            Core.Just ("ServerPath" Core..= serverPath),
            Core.Just ("HttpHeaders" Core..= httpHeaders)
          ]
      )
