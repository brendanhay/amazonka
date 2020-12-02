{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationFilterOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationFilterOperatorType where

import Network.AWS.Prelude

data AssociationFilterOperatorType
  = AFOTEqual
  | AFOTGreaterThan
  | AFOTLessThan
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

instance FromText AssociationFilterOperatorType where
  parser =
    takeLowerText >>= \case
      "equal" -> pure AFOTEqual
      "greater_than" -> pure AFOTGreaterThan
      "less_than" -> pure AFOTLessThan
      e ->
        fromTextError $
          "Failure parsing AssociationFilterOperatorType from value: '" <> e
            <> "'. Accepted values: equal, greater_than, less_than"

instance ToText AssociationFilterOperatorType where
  toText = \case
    AFOTEqual -> "EQUAL"
    AFOTGreaterThan -> "GREATER_THAN"
    AFOTLessThan -> "LESS_THAN"

instance Hashable AssociationFilterOperatorType

instance NFData AssociationFilterOperatorType

instance ToByteString AssociationFilterOperatorType

instance ToQuery AssociationFilterOperatorType

instance ToHeader AssociationFilterOperatorType

instance ToJSON AssociationFilterOperatorType where
  toJSON = toJSONText
