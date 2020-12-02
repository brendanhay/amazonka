{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.Sum where

import Network.AWS.Prelude

data PolicyComplianceStatusType
  = Compliant
  | NonCompliant
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PolicyComplianceStatusType where
    parser = takeLowerText >>= \case
        "compliant" -> pure Compliant
        "non_compliant" -> pure NonCompliant
        e -> fromTextError $ "Failure parsing PolicyComplianceStatusType from value: '" <> e
           <> "'. Accepted values: compliant, non_compliant"

instance ToText PolicyComplianceStatusType where
    toText = \case
        Compliant -> "COMPLIANT"
        NonCompliant -> "NON_COMPLIANT"

instance Hashable     PolicyComplianceStatusType
instance NFData       PolicyComplianceStatusType
instance ToByteString PolicyComplianceStatusType
instance ToQuery      PolicyComplianceStatusType
instance ToHeader     PolicyComplianceStatusType

instance FromJSON PolicyComplianceStatusType where
    parseJSON = parseJSONText "PolicyComplianceStatusType"

data SecurityServiceType =
  Waf
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SecurityServiceType where
    parser = takeLowerText >>= \case
        "waf" -> pure Waf
        e -> fromTextError $ "Failure parsing SecurityServiceType from value: '" <> e
           <> "'. Accepted values: waf"

instance ToText SecurityServiceType where
    toText = \case
        Waf -> "WAF"

instance Hashable     SecurityServiceType
instance NFData       SecurityServiceType
instance ToByteString SecurityServiceType
instance ToQuery      SecurityServiceType
instance ToHeader     SecurityServiceType

instance ToJSON SecurityServiceType where
    toJSON = toJSONText

instance FromJSON SecurityServiceType where
    parseJSON = parseJSONText "SecurityServiceType"

data ViolationReason
  = ResourceIncorrectWebACL
  | ResourceMissingWebACL
  | WebACLMissingRuleGroup
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ViolationReason where
    parser = takeLowerText >>= \case
        "resource_incorrect_web_acl" -> pure ResourceIncorrectWebACL
        "resource_missing_web_acl" -> pure ResourceMissingWebACL
        "web_acl_missing_rule_group" -> pure WebACLMissingRuleGroup
        e -> fromTextError $ "Failure parsing ViolationReason from value: '" <> e
           <> "'. Accepted values: resource_incorrect_web_acl, resource_missing_web_acl, web_acl_missing_rule_group"

instance ToText ViolationReason where
    toText = \case
        ResourceIncorrectWebACL -> "RESOURCE_INCORRECT_WEB_ACL"
        ResourceMissingWebACL -> "RESOURCE_MISSING_WEB_ACL"
        WebACLMissingRuleGroup -> "WEB_ACL_MISSING_RULE_GROUP"

instance Hashable     ViolationReason
instance NFData       ViolationReason
instance ToByteString ViolationReason
instance ToQuery      ViolationReason
instance ToHeader     ViolationReason

instance FromJSON ViolationReason where
    parseJSON = parseJSONText "ViolationReason"
