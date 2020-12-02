{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ScopeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ScopeType where

import Network.AWS.Prelude

data ScopeType
  = InstanceId
  | RulesPackageARN
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

instance FromText ScopeType where
  parser =
    takeLowerText >>= \case
      "instance_id" -> pure InstanceId
      "rules_package_arn" -> pure RulesPackageARN
      e ->
        fromTextError $
          "Failure parsing ScopeType from value: '" <> e
            <> "'. Accepted values: instance_id, rules_package_arn"

instance ToText ScopeType where
  toText = \case
    InstanceId -> "INSTANCE_ID"
    RulesPackageARN -> "RULES_PACKAGE_ARN"

instance Hashable ScopeType

instance NFData ScopeType

instance ToByteString ScopeType

instance ToQuery ScopeType

instance ToHeader ScopeType

instance FromJSON ScopeType where
  parseJSON = parseJSONText "ScopeType"
