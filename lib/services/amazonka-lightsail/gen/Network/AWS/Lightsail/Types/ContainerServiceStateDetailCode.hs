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
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceStateDetailCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceStateDetailCode
  ( ContainerServiceStateDetailCode
      ( ..,
        ContainerServiceStateDetailCode_ACTIVATING_DEPLOYMENT,
        ContainerServiceStateDetailCode_CERTIFICATE_LIMIT_EXCEEDED,
        ContainerServiceStateDetailCode_CREATING_DEPLOYMENT,
        ContainerServiceStateDetailCode_CREATING_NETWORK_INFRASTRUCTURE,
        ContainerServiceStateDetailCode_CREATING_SYSTEM_RESOURCES,
        ContainerServiceStateDetailCode_EVALUATING_HEALTH_CHECK,
        ContainerServiceStateDetailCode_PROVISIONING_CERTIFICATE,
        ContainerServiceStateDetailCode_PROVISIONING_SERVICE,
        ContainerServiceStateDetailCode_UNKNOWN_ERROR
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ContainerServiceStateDetailCode = ContainerServiceStateDetailCode'
  { fromContainerServiceStateDetailCode ::
      Core.Text
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

pattern ContainerServiceStateDetailCode_ACTIVATING_DEPLOYMENT :: ContainerServiceStateDetailCode
pattern ContainerServiceStateDetailCode_ACTIVATING_DEPLOYMENT = ContainerServiceStateDetailCode' "ACTIVATING_DEPLOYMENT"

pattern ContainerServiceStateDetailCode_CERTIFICATE_LIMIT_EXCEEDED :: ContainerServiceStateDetailCode
pattern ContainerServiceStateDetailCode_CERTIFICATE_LIMIT_EXCEEDED = ContainerServiceStateDetailCode' "CERTIFICATE_LIMIT_EXCEEDED"

pattern ContainerServiceStateDetailCode_CREATING_DEPLOYMENT :: ContainerServiceStateDetailCode
pattern ContainerServiceStateDetailCode_CREATING_DEPLOYMENT = ContainerServiceStateDetailCode' "CREATING_DEPLOYMENT"

pattern ContainerServiceStateDetailCode_CREATING_NETWORK_INFRASTRUCTURE :: ContainerServiceStateDetailCode
pattern ContainerServiceStateDetailCode_CREATING_NETWORK_INFRASTRUCTURE = ContainerServiceStateDetailCode' "CREATING_NETWORK_INFRASTRUCTURE"

pattern ContainerServiceStateDetailCode_CREATING_SYSTEM_RESOURCES :: ContainerServiceStateDetailCode
pattern ContainerServiceStateDetailCode_CREATING_SYSTEM_RESOURCES = ContainerServiceStateDetailCode' "CREATING_SYSTEM_RESOURCES"

pattern ContainerServiceStateDetailCode_EVALUATING_HEALTH_CHECK :: ContainerServiceStateDetailCode
pattern ContainerServiceStateDetailCode_EVALUATING_HEALTH_CHECK = ContainerServiceStateDetailCode' "EVALUATING_HEALTH_CHECK"

pattern ContainerServiceStateDetailCode_PROVISIONING_CERTIFICATE :: ContainerServiceStateDetailCode
pattern ContainerServiceStateDetailCode_PROVISIONING_CERTIFICATE = ContainerServiceStateDetailCode' "PROVISIONING_CERTIFICATE"

pattern ContainerServiceStateDetailCode_PROVISIONING_SERVICE :: ContainerServiceStateDetailCode
pattern ContainerServiceStateDetailCode_PROVISIONING_SERVICE = ContainerServiceStateDetailCode' "PROVISIONING_SERVICE"

pattern ContainerServiceStateDetailCode_UNKNOWN_ERROR :: ContainerServiceStateDetailCode
pattern ContainerServiceStateDetailCode_UNKNOWN_ERROR = ContainerServiceStateDetailCode' "UNKNOWN_ERROR"

{-# COMPLETE
  ContainerServiceStateDetailCode_ACTIVATING_DEPLOYMENT,
  ContainerServiceStateDetailCode_CERTIFICATE_LIMIT_EXCEEDED,
  ContainerServiceStateDetailCode_CREATING_DEPLOYMENT,
  ContainerServiceStateDetailCode_CREATING_NETWORK_INFRASTRUCTURE,
  ContainerServiceStateDetailCode_CREATING_SYSTEM_RESOURCES,
  ContainerServiceStateDetailCode_EVALUATING_HEALTH_CHECK,
  ContainerServiceStateDetailCode_PROVISIONING_CERTIFICATE,
  ContainerServiceStateDetailCode_PROVISIONING_SERVICE,
  ContainerServiceStateDetailCode_UNKNOWN_ERROR,
  ContainerServiceStateDetailCode'
  #-}
