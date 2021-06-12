{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_ACCOUNT_SETTINGS,
        ResourceType_CA_CERTIFICATE,
        ResourceType_CLIENT_ID,
        ResourceType_COGNITO_IDENTITY_POOL,
        ResourceType_DEVICE_CERTIFICATE,
        ResourceType_IAM_ROLE,
        ResourceType_IOT_POLICY,
        ResourceType_ROLE_ALIAS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ResourceType = ResourceType'
  { fromResourceType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ResourceType_ACCOUNT_SETTINGS :: ResourceType
pattern ResourceType_ACCOUNT_SETTINGS = ResourceType' "ACCOUNT_SETTINGS"

pattern ResourceType_CA_CERTIFICATE :: ResourceType
pattern ResourceType_CA_CERTIFICATE = ResourceType' "CA_CERTIFICATE"

pattern ResourceType_CLIENT_ID :: ResourceType
pattern ResourceType_CLIENT_ID = ResourceType' "CLIENT_ID"

pattern ResourceType_COGNITO_IDENTITY_POOL :: ResourceType
pattern ResourceType_COGNITO_IDENTITY_POOL = ResourceType' "COGNITO_IDENTITY_POOL"

pattern ResourceType_DEVICE_CERTIFICATE :: ResourceType
pattern ResourceType_DEVICE_CERTIFICATE = ResourceType' "DEVICE_CERTIFICATE"

pattern ResourceType_IAM_ROLE :: ResourceType
pattern ResourceType_IAM_ROLE = ResourceType' "IAM_ROLE"

pattern ResourceType_IOT_POLICY :: ResourceType
pattern ResourceType_IOT_POLICY = ResourceType' "IOT_POLICY"

pattern ResourceType_ROLE_ALIAS :: ResourceType
pattern ResourceType_ROLE_ALIAS = ResourceType' "ROLE_ALIAS"

{-# COMPLETE
  ResourceType_ACCOUNT_SETTINGS,
  ResourceType_CA_CERTIFICATE,
  ResourceType_CLIENT_ID,
  ResourceType_COGNITO_IDENTITY_POOL,
  ResourceType_DEVICE_CERTIFICATE,
  ResourceType_IAM_ROLE,
  ResourceType_IOT_POLICY,
  ResourceType_ROLE_ALIAS,
  ResourceType'
  #-}
