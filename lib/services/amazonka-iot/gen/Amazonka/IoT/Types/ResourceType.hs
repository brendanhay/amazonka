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
-- Module      : Amazonka.IoT.Types.ResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_ACCOUNT_SETTINGS,
        ResourceType_CA_CERTIFICATE,
        ResourceType_CLIENT_ID,
        ResourceType_COGNITO_IDENTITY_POOL,
        ResourceType_DEVICE_CERTIFICATE,
        ResourceType_IAM_ROLE,
        ResourceType_IOT_POLICY,
        ResourceType_ISSUER_CERTIFICATE,
        ResourceType_ROLE_ALIAS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

pattern ResourceType_ISSUER_CERTIFICATE :: ResourceType
pattern ResourceType_ISSUER_CERTIFICATE = ResourceType' "ISSUER_CERTIFICATE"

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
  ResourceType_ISSUER_CERTIFICATE,
  ResourceType_ROLE_ALIAS,
  ResourceType'
  #-}
