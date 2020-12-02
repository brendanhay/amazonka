{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceQueryOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceQueryOperatorType where

import Network.AWS.Prelude

data ComplianceQueryOperatorType
  = CQOTBeginWith
  | CQOTEqual
  | CQOTGreaterThan
  | CQOTLessThan
  | CQOTNotEqual
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

instance FromText ComplianceQueryOperatorType where
  parser =
    takeLowerText >>= \case
      "begin_with" -> pure CQOTBeginWith
      "equal" -> pure CQOTEqual
      "greater_than" -> pure CQOTGreaterThan
      "less_than" -> pure CQOTLessThan
      "not_equal" -> pure CQOTNotEqual
      e ->
        fromTextError $
          "Failure parsing ComplianceQueryOperatorType from value: '" <> e
            <> "'. Accepted values: begin_with, equal, greater_than, less_than, not_equal"

instance ToText ComplianceQueryOperatorType where
  toText = \case
    CQOTBeginWith -> "BEGIN_WITH"
    CQOTEqual -> "EQUAL"
    CQOTGreaterThan -> "GREATER_THAN"
    CQOTLessThan -> "LESS_THAN"
    CQOTNotEqual -> "NOT_EQUAL"

instance Hashable ComplianceQueryOperatorType

instance NFData ComplianceQueryOperatorType

instance ToByteString ComplianceQueryOperatorType

instance ToQuery ComplianceQueryOperatorType

instance ToHeader ComplianceQueryOperatorType

instance ToJSON ComplianceQueryOperatorType where
  toJSON = toJSONText
