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
-- Module      : Network.AWS.AppStream.Types.FleetAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetAttribute
  ( FleetAttribute
      ( ..,
        FleetAttribute_DOMAIN_JOIN_INFO,
        FleetAttribute_IAM_ROLE_ARN,
        FleetAttribute_VPC_CONFIGURATION,
        FleetAttribute_VPC_CONFIGURATION_SECURITY_GROUP_IDS
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The fleet attribute.
newtype FleetAttribute = FleetAttribute'
  { fromFleetAttribute ::
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

pattern FleetAttribute_DOMAIN_JOIN_INFO :: FleetAttribute
pattern FleetAttribute_DOMAIN_JOIN_INFO = FleetAttribute' "DOMAIN_JOIN_INFO"

pattern FleetAttribute_IAM_ROLE_ARN :: FleetAttribute
pattern FleetAttribute_IAM_ROLE_ARN = FleetAttribute' "IAM_ROLE_ARN"

pattern FleetAttribute_VPC_CONFIGURATION :: FleetAttribute
pattern FleetAttribute_VPC_CONFIGURATION = FleetAttribute' "VPC_CONFIGURATION"

pattern FleetAttribute_VPC_CONFIGURATION_SECURITY_GROUP_IDS :: FleetAttribute
pattern FleetAttribute_VPC_CONFIGURATION_SECURITY_GROUP_IDS = FleetAttribute' "VPC_CONFIGURATION_SECURITY_GROUP_IDS"

{-# COMPLETE
  FleetAttribute_DOMAIN_JOIN_INFO,
  FleetAttribute_IAM_ROLE_ARN,
  FleetAttribute_VPC_CONFIGURATION,
  FleetAttribute_VPC_CONFIGURATION_SECURITY_GROUP_IDS,
  FleetAttribute'
  #-}
