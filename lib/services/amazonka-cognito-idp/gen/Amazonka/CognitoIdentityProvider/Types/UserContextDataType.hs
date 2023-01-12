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
-- Module      : Amazonka.CognitoIdentityProvider.Types.UserContextDataType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UserContextDataType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contextual data, such as the user\'s device fingerprint, IP address, or
-- location, used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
--
-- /See:/ 'newUserContextDataType' smart constructor.
data UserContextDataType = UserContextDataType'
  { -- | Encoded device-fingerprint details that your app collected with the
    -- Amazon Cognito context data collection library. For more information,
    -- see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-adaptive-authentication.html#user-pool-settings-adaptive-authentication-device-fingerprint Adding user device and session data to API requests>.
    encodedData :: Prelude.Maybe Prelude.Text,
    -- | The source IP address of your user\'s device.
    ipAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserContextDataType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encodedData', 'userContextDataType_encodedData' - Encoded device-fingerprint details that your app collected with the
-- Amazon Cognito context data collection library. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-adaptive-authentication.html#user-pool-settings-adaptive-authentication-device-fingerprint Adding user device and session data to API requests>.
--
-- 'ipAddress', 'userContextDataType_ipAddress' - The source IP address of your user\'s device.
newUserContextDataType ::
  UserContextDataType
newUserContextDataType =
  UserContextDataType'
    { encodedData = Prelude.Nothing,
      ipAddress = Prelude.Nothing
    }

-- | Encoded device-fingerprint details that your app collected with the
-- Amazon Cognito context data collection library. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-adaptive-authentication.html#user-pool-settings-adaptive-authentication-device-fingerprint Adding user device and session data to API requests>.
userContextDataType_encodedData :: Lens.Lens' UserContextDataType (Prelude.Maybe Prelude.Text)
userContextDataType_encodedData = Lens.lens (\UserContextDataType' {encodedData} -> encodedData) (\s@UserContextDataType' {} a -> s {encodedData = a} :: UserContextDataType)

-- | The source IP address of your user\'s device.
userContextDataType_ipAddress :: Lens.Lens' UserContextDataType (Prelude.Maybe Prelude.Text)
userContextDataType_ipAddress = Lens.lens (\UserContextDataType' {ipAddress} -> ipAddress) (\s@UserContextDataType' {} a -> s {ipAddress = a} :: UserContextDataType)

instance Prelude.Hashable UserContextDataType where
  hashWithSalt _salt UserContextDataType' {..} =
    _salt `Prelude.hashWithSalt` encodedData
      `Prelude.hashWithSalt` ipAddress

instance Prelude.NFData UserContextDataType where
  rnf UserContextDataType' {..} =
    Prelude.rnf encodedData
      `Prelude.seq` Prelude.rnf ipAddress

instance Data.ToJSON UserContextDataType where
  toJSON UserContextDataType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncodedData" Data..=) Prelude.<$> encodedData,
            ("IpAddress" Data..=) Prelude.<$> ipAddress
          ]
      )
