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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceStateDetailCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceStateDetailCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContainerServiceStateDetailCode = ContainerServiceStateDetailCode'
  { fromContainerServiceStateDetailCode ::
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
