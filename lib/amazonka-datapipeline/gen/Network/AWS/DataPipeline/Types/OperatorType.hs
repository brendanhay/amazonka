{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.OperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.OperatorType where

import Network.AWS.Prelude

data OperatorType
  = OperatorBetween
  | OperatorEQ'
  | OperatorGE
  | OperatorLE
  | OperatorRefEQ
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

instance FromText OperatorType where
  parser =
    takeLowerText >>= \case
      "between" -> pure OperatorBetween
      "eq" -> pure OperatorEQ'
      "ge" -> pure OperatorGE
      "le" -> pure OperatorLE
      "ref_eq" -> pure OperatorRefEQ
      e ->
        fromTextError $
          "Failure parsing OperatorType from value: '" <> e
            <> "'. Accepted values: between, eq, ge, le, ref_eq"

instance ToText OperatorType where
  toText = \case
    OperatorBetween -> "BETWEEN"
    OperatorEQ' -> "EQ"
    OperatorGE -> "GE"
    OperatorLE -> "LE"
    OperatorRefEQ -> "REF_EQ"

instance Hashable OperatorType

instance NFData OperatorType

instance ToByteString OperatorType

instance ToQuery OperatorType

instance ToHeader OperatorType

instance ToJSON OperatorType where
  toJSON = toJSONText
