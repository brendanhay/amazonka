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
-- Module      : Amazonka.AppStream.Types.FleetAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.FleetAttribute
  ( FleetAttribute
      ( ..,
        FleetAttribute_DOMAIN_JOIN_INFO,
        FleetAttribute_IAM_ROLE_ARN,
        FleetAttribute_SESSION_SCRIPT_S3_LOCATION,
        FleetAttribute_USB_DEVICE_FILTER_STRINGS,
        FleetAttribute_VPC_CONFIGURATION,
        FleetAttribute_VPC_CONFIGURATION_SECURITY_GROUP_IDS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The fleet attribute.
newtype FleetAttribute = FleetAttribute'
  { fromFleetAttribute ::
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

pattern FleetAttribute_DOMAIN_JOIN_INFO :: FleetAttribute
pattern FleetAttribute_DOMAIN_JOIN_INFO = FleetAttribute' "DOMAIN_JOIN_INFO"

pattern FleetAttribute_IAM_ROLE_ARN :: FleetAttribute
pattern FleetAttribute_IAM_ROLE_ARN = FleetAttribute' "IAM_ROLE_ARN"

pattern FleetAttribute_SESSION_SCRIPT_S3_LOCATION :: FleetAttribute
pattern FleetAttribute_SESSION_SCRIPT_S3_LOCATION = FleetAttribute' "SESSION_SCRIPT_S3_LOCATION"

pattern FleetAttribute_USB_DEVICE_FILTER_STRINGS :: FleetAttribute
pattern FleetAttribute_USB_DEVICE_FILTER_STRINGS = FleetAttribute' "USB_DEVICE_FILTER_STRINGS"

pattern FleetAttribute_VPC_CONFIGURATION :: FleetAttribute
pattern FleetAttribute_VPC_CONFIGURATION = FleetAttribute' "VPC_CONFIGURATION"

pattern FleetAttribute_VPC_CONFIGURATION_SECURITY_GROUP_IDS :: FleetAttribute
pattern FleetAttribute_VPC_CONFIGURATION_SECURITY_GROUP_IDS = FleetAttribute' "VPC_CONFIGURATION_SECURITY_GROUP_IDS"

{-# COMPLETE
  FleetAttribute_DOMAIN_JOIN_INFO,
  FleetAttribute_IAM_ROLE_ARN,
  FleetAttribute_SESSION_SCRIPT_S3_LOCATION,
  FleetAttribute_USB_DEVICE_FILTER_STRINGS,
  FleetAttribute_VPC_CONFIGURATION,
  FleetAttribute_VPC_CONFIGURATION_SECURITY_GROUP_IDS,
  FleetAttribute'
  #-}
