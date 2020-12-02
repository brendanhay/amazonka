{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.Sum where

import Network.AWS.Prelude

data AmbiguousRoleResolutionType
  = AuthenticatedRole
  | Deny
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AmbiguousRoleResolutionType where
    parser = takeLowerText >>= \case
        "authenticatedrole" -> pure AuthenticatedRole
        "deny" -> pure Deny
        e -> fromTextError $ "Failure parsing AmbiguousRoleResolutionType from value: '" <> e
           <> "'. Accepted values: authenticatedrole, deny"

instance ToText AmbiguousRoleResolutionType where
    toText = \case
        AuthenticatedRole -> "AuthenticatedRole"
        Deny -> "Deny"

instance Hashable     AmbiguousRoleResolutionType
instance NFData       AmbiguousRoleResolutionType
instance ToByteString AmbiguousRoleResolutionType
instance ToQuery      AmbiguousRoleResolutionType
instance ToHeader     AmbiguousRoleResolutionType

instance ToJSON AmbiguousRoleResolutionType where
    toJSON = toJSONText

instance FromJSON AmbiguousRoleResolutionType where
    parseJSON = parseJSONText "AmbiguousRoleResolutionType"

data CognitoErrorCode
  = AccessDenied
  | InternalServerError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CognitoErrorCode where
    parser = takeLowerText >>= \case
        "accessdenied" -> pure AccessDenied
        "internalservererror" -> pure InternalServerError
        e -> fromTextError $ "Failure parsing CognitoErrorCode from value: '" <> e
           <> "'. Accepted values: accessdenied, internalservererror"

instance ToText CognitoErrorCode where
    toText = \case
        AccessDenied -> "AccessDenied"
        InternalServerError -> "InternalServerError"

instance Hashable     CognitoErrorCode
instance NFData       CognitoErrorCode
instance ToByteString CognitoErrorCode
instance ToQuery      CognitoErrorCode
instance ToHeader     CognitoErrorCode

instance FromJSON CognitoErrorCode where
    parseJSON = parseJSONText "CognitoErrorCode"

data MappingRuleMatchType
  = Contains
  | Equals
  | NotEqual
  | StartsWith
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MappingRuleMatchType where
    parser = takeLowerText >>= \case
        "contains" -> pure Contains
        "equals" -> pure Equals
        "notequal" -> pure NotEqual
        "startswith" -> pure StartsWith
        e -> fromTextError $ "Failure parsing MappingRuleMatchType from value: '" <> e
           <> "'. Accepted values: contains, equals, notequal, startswith"

instance ToText MappingRuleMatchType where
    toText = \case
        Contains -> "Contains"
        Equals -> "Equals"
        NotEqual -> "NotEqual"
        StartsWith -> "StartsWith"

instance Hashable     MappingRuleMatchType
instance NFData       MappingRuleMatchType
instance ToByteString MappingRuleMatchType
instance ToQuery      MappingRuleMatchType
instance ToHeader     MappingRuleMatchType

instance ToJSON MappingRuleMatchType where
    toJSON = toJSONText

instance FromJSON MappingRuleMatchType where
    parseJSON = parseJSONText "MappingRuleMatchType"

data RoleMappingType
  = Rules
  | Token
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RoleMappingType where
    parser = takeLowerText >>= \case
        "rules" -> pure Rules
        "token" -> pure Token
        e -> fromTextError $ "Failure parsing RoleMappingType from value: '" <> e
           <> "'. Accepted values: rules, token"

instance ToText RoleMappingType where
    toText = \case
        Rules -> "Rules"
        Token -> "Token"

instance Hashable     RoleMappingType
instance NFData       RoleMappingType
instance ToByteString RoleMappingType
instance ToQuery      RoleMappingType
instance ToHeader     RoleMappingType

instance ToJSON RoleMappingType where
    toJSON = toJSONText

instance FromJSON RoleMappingType where
    parseJSON = parseJSONText "RoleMappingType"
