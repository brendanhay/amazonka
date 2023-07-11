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
-- Module      : Amazonka.FMS.Types.ViolationReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ViolationReason
  ( ViolationReason
      ( ..,
        ViolationReason_BLACK_HOLE_ROUTE_DETECTED,
        ViolationReason_BLACK_HOLE_ROUTE_DETECTED_IN_FIREWALL_SUBNET,
        ViolationReason_FIREWALL_SUBNET_IS_OUT_OF_SCOPE,
        ViolationReason_FIREWALL_SUBNET_MISSING_EXPECTED_ROUTE,
        ViolationReason_FIREWALL_SUBNET_MISSING_VPCE_ENDPOINT,
        ViolationReason_FMS_CREATED_SECURITY_GROUP_EDITED,
        ViolationReason_INTERNET_GATEWAY_MISSING_EXPECTED_ROUTE,
        ViolationReason_INTERNET_TRAFFIC_NOT_INSPECTED,
        ViolationReason_INVALID_ROUTE_CONFIGURATION,
        ViolationReason_MISSING_EXPECTED_ROUTE_TABLE,
        ViolationReason_MISSING_FIREWALL,
        ViolationReason_MISSING_FIREWALL_SUBNET_IN_AZ,
        ViolationReason_MISSING_TARGET_GATEWAY,
        ViolationReason_NETWORK_FIREWALL_POLICY_MODIFIED,
        ViolationReason_RESOURCE_INCORRECT_WEB_ACL,
        ViolationReason_RESOURCE_MISSING_DNS_FIREWALL,
        ViolationReason_RESOURCE_MISSING_SECURITY_GROUP,
        ViolationReason_RESOURCE_MISSING_SHIELD_PROTECTION,
        ViolationReason_RESOURCE_MISSING_WEB_ACL,
        ViolationReason_RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION,
        ViolationReason_RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP,
        ViolationReason_ROUTE_HAS_OUT_OF_SCOPE_ENDPOINT,
        ViolationReason_SECURITY_GROUP_REDUNDANT,
        ViolationReason_SECURITY_GROUP_UNUSED,
        ViolationReason_TRAFFIC_INSPECTION_CROSSES_AZ_BOUNDARY,
        ViolationReason_UNEXPECTED_FIREWALL_ROUTES,
        ViolationReason_UNEXPECTED_TARGET_GATEWAY_ROUTES,
        ViolationReason_WEB_ACL_MISSING_RULE_GROUP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ViolationReason = ViolationReason'
  { fromViolationReason ::
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

pattern ViolationReason_BLACK_HOLE_ROUTE_DETECTED :: ViolationReason
pattern ViolationReason_BLACK_HOLE_ROUTE_DETECTED = ViolationReason' "BLACK_HOLE_ROUTE_DETECTED"

pattern ViolationReason_BLACK_HOLE_ROUTE_DETECTED_IN_FIREWALL_SUBNET :: ViolationReason
pattern ViolationReason_BLACK_HOLE_ROUTE_DETECTED_IN_FIREWALL_SUBNET = ViolationReason' "BLACK_HOLE_ROUTE_DETECTED_IN_FIREWALL_SUBNET"

pattern ViolationReason_FIREWALL_SUBNET_IS_OUT_OF_SCOPE :: ViolationReason
pattern ViolationReason_FIREWALL_SUBNET_IS_OUT_OF_SCOPE = ViolationReason' "FIREWALL_SUBNET_IS_OUT_OF_SCOPE"

pattern ViolationReason_FIREWALL_SUBNET_MISSING_EXPECTED_ROUTE :: ViolationReason
pattern ViolationReason_FIREWALL_SUBNET_MISSING_EXPECTED_ROUTE = ViolationReason' "FIREWALL_SUBNET_MISSING_EXPECTED_ROUTE"

pattern ViolationReason_FIREWALL_SUBNET_MISSING_VPCE_ENDPOINT :: ViolationReason
pattern ViolationReason_FIREWALL_SUBNET_MISSING_VPCE_ENDPOINT = ViolationReason' "FIREWALL_SUBNET_MISSING_VPCE_ENDPOINT"

pattern ViolationReason_FMS_CREATED_SECURITY_GROUP_EDITED :: ViolationReason
pattern ViolationReason_FMS_CREATED_SECURITY_GROUP_EDITED = ViolationReason' "FMS_CREATED_SECURITY_GROUP_EDITED"

pattern ViolationReason_INTERNET_GATEWAY_MISSING_EXPECTED_ROUTE :: ViolationReason
pattern ViolationReason_INTERNET_GATEWAY_MISSING_EXPECTED_ROUTE = ViolationReason' "INTERNET_GATEWAY_MISSING_EXPECTED_ROUTE"

pattern ViolationReason_INTERNET_TRAFFIC_NOT_INSPECTED :: ViolationReason
pattern ViolationReason_INTERNET_TRAFFIC_NOT_INSPECTED = ViolationReason' "INTERNET_TRAFFIC_NOT_INSPECTED"

pattern ViolationReason_INVALID_ROUTE_CONFIGURATION :: ViolationReason
pattern ViolationReason_INVALID_ROUTE_CONFIGURATION = ViolationReason' "INVALID_ROUTE_CONFIGURATION"

pattern ViolationReason_MISSING_EXPECTED_ROUTE_TABLE :: ViolationReason
pattern ViolationReason_MISSING_EXPECTED_ROUTE_TABLE = ViolationReason' "MISSING_EXPECTED_ROUTE_TABLE"

pattern ViolationReason_MISSING_FIREWALL :: ViolationReason
pattern ViolationReason_MISSING_FIREWALL = ViolationReason' "MISSING_FIREWALL"

pattern ViolationReason_MISSING_FIREWALL_SUBNET_IN_AZ :: ViolationReason
pattern ViolationReason_MISSING_FIREWALL_SUBNET_IN_AZ = ViolationReason' "MISSING_FIREWALL_SUBNET_IN_AZ"

pattern ViolationReason_MISSING_TARGET_GATEWAY :: ViolationReason
pattern ViolationReason_MISSING_TARGET_GATEWAY = ViolationReason' "MISSING_TARGET_GATEWAY"

pattern ViolationReason_NETWORK_FIREWALL_POLICY_MODIFIED :: ViolationReason
pattern ViolationReason_NETWORK_FIREWALL_POLICY_MODIFIED = ViolationReason' "NETWORK_FIREWALL_POLICY_MODIFIED"

pattern ViolationReason_RESOURCE_INCORRECT_WEB_ACL :: ViolationReason
pattern ViolationReason_RESOURCE_INCORRECT_WEB_ACL = ViolationReason' "RESOURCE_INCORRECT_WEB_ACL"

pattern ViolationReason_RESOURCE_MISSING_DNS_FIREWALL :: ViolationReason
pattern ViolationReason_RESOURCE_MISSING_DNS_FIREWALL = ViolationReason' "RESOURCE_MISSING_DNS_FIREWALL"

pattern ViolationReason_RESOURCE_MISSING_SECURITY_GROUP :: ViolationReason
pattern ViolationReason_RESOURCE_MISSING_SECURITY_GROUP = ViolationReason' "RESOURCE_MISSING_SECURITY_GROUP"

pattern ViolationReason_RESOURCE_MISSING_SHIELD_PROTECTION :: ViolationReason
pattern ViolationReason_RESOURCE_MISSING_SHIELD_PROTECTION = ViolationReason' "RESOURCE_MISSING_SHIELD_PROTECTION"

pattern ViolationReason_RESOURCE_MISSING_WEB_ACL :: ViolationReason
pattern ViolationReason_RESOURCE_MISSING_WEB_ACL = ViolationReason' "RESOURCE_MISSING_WEB_ACL"

pattern ViolationReason_RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION :: ViolationReason
pattern ViolationReason_RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION = ViolationReason' "RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION"

pattern ViolationReason_RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP :: ViolationReason
pattern ViolationReason_RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP = ViolationReason' "RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP"

pattern ViolationReason_ROUTE_HAS_OUT_OF_SCOPE_ENDPOINT :: ViolationReason
pattern ViolationReason_ROUTE_HAS_OUT_OF_SCOPE_ENDPOINT = ViolationReason' "ROUTE_HAS_OUT_OF_SCOPE_ENDPOINT"

pattern ViolationReason_SECURITY_GROUP_REDUNDANT :: ViolationReason
pattern ViolationReason_SECURITY_GROUP_REDUNDANT = ViolationReason' "SECURITY_GROUP_REDUNDANT"

pattern ViolationReason_SECURITY_GROUP_UNUSED :: ViolationReason
pattern ViolationReason_SECURITY_GROUP_UNUSED = ViolationReason' "SECURITY_GROUP_UNUSED"

pattern ViolationReason_TRAFFIC_INSPECTION_CROSSES_AZ_BOUNDARY :: ViolationReason
pattern ViolationReason_TRAFFIC_INSPECTION_CROSSES_AZ_BOUNDARY = ViolationReason' "TRAFFIC_INSPECTION_CROSSES_AZ_BOUNDARY"

pattern ViolationReason_UNEXPECTED_FIREWALL_ROUTES :: ViolationReason
pattern ViolationReason_UNEXPECTED_FIREWALL_ROUTES = ViolationReason' "UNEXPECTED_FIREWALL_ROUTES"

pattern ViolationReason_UNEXPECTED_TARGET_GATEWAY_ROUTES :: ViolationReason
pattern ViolationReason_UNEXPECTED_TARGET_GATEWAY_ROUTES = ViolationReason' "UNEXPECTED_TARGET_GATEWAY_ROUTES"

pattern ViolationReason_WEB_ACL_MISSING_RULE_GROUP :: ViolationReason
pattern ViolationReason_WEB_ACL_MISSING_RULE_GROUP = ViolationReason' "WEB_ACL_MISSING_RULE_GROUP"

{-# COMPLETE
  ViolationReason_BLACK_HOLE_ROUTE_DETECTED,
  ViolationReason_BLACK_HOLE_ROUTE_DETECTED_IN_FIREWALL_SUBNET,
  ViolationReason_FIREWALL_SUBNET_IS_OUT_OF_SCOPE,
  ViolationReason_FIREWALL_SUBNET_MISSING_EXPECTED_ROUTE,
  ViolationReason_FIREWALL_SUBNET_MISSING_VPCE_ENDPOINT,
  ViolationReason_FMS_CREATED_SECURITY_GROUP_EDITED,
  ViolationReason_INTERNET_GATEWAY_MISSING_EXPECTED_ROUTE,
  ViolationReason_INTERNET_TRAFFIC_NOT_INSPECTED,
  ViolationReason_INVALID_ROUTE_CONFIGURATION,
  ViolationReason_MISSING_EXPECTED_ROUTE_TABLE,
  ViolationReason_MISSING_FIREWALL,
  ViolationReason_MISSING_FIREWALL_SUBNET_IN_AZ,
  ViolationReason_MISSING_TARGET_GATEWAY,
  ViolationReason_NETWORK_FIREWALL_POLICY_MODIFIED,
  ViolationReason_RESOURCE_INCORRECT_WEB_ACL,
  ViolationReason_RESOURCE_MISSING_DNS_FIREWALL,
  ViolationReason_RESOURCE_MISSING_SECURITY_GROUP,
  ViolationReason_RESOURCE_MISSING_SHIELD_PROTECTION,
  ViolationReason_RESOURCE_MISSING_WEB_ACL,
  ViolationReason_RESOURCE_MISSING_WEB_ACL_OR_SHIELD_PROTECTION,
  ViolationReason_RESOURCE_VIOLATES_AUDIT_SECURITY_GROUP,
  ViolationReason_ROUTE_HAS_OUT_OF_SCOPE_ENDPOINT,
  ViolationReason_SECURITY_GROUP_REDUNDANT,
  ViolationReason_SECURITY_GROUP_UNUSED,
  ViolationReason_TRAFFIC_INSPECTION_CROSSES_AZ_BOUNDARY,
  ViolationReason_UNEXPECTED_FIREWALL_ROUTES,
  ViolationReason_UNEXPECTED_TARGET_GATEWAY_ROUTES,
  ViolationReason_WEB_ACL_MISSING_RULE_GROUP,
  ViolationReason'
  #-}
