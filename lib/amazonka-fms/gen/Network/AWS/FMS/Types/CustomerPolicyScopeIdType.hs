{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.CustomerPolicyScopeIdType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.CustomerPolicyScopeIdType where

import Network.AWS.Prelude

data CustomerPolicyScopeIdType
  = Account
  | OrgUnit
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText CustomerPolicyScopeIdType where
  parser =
    takeLowerText >>= \case
      "account" -> pure Account
      "org_unit" -> pure OrgUnit
      e ->
        fromTextError $
          "Failure parsing CustomerPolicyScopeIdType from value: '" <> e
            <> "'. Accepted values: account, org_unit"

instance ToText CustomerPolicyScopeIdType where
  toText = \case
    Account -> "ACCOUNT"
    OrgUnit -> "ORG_UNIT"

instance Hashable CustomerPolicyScopeIdType

instance NFData CustomerPolicyScopeIdType

instance ToByteString CustomerPolicyScopeIdType

instance ToQuery CustomerPolicyScopeIdType

instance ToHeader CustomerPolicyScopeIdType

instance ToJSON CustomerPolicyScopeIdType where
  toJSON = toJSONText

instance FromJSON CustomerPolicyScopeIdType where
  parseJSON = parseJSONText "CustomerPolicyScopeIdType"
