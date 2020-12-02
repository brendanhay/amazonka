{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.OperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.OperatorType where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data OperatorType
  = Between
  | EQ'
  | GE
  | GT'
  | IN
  | LE
  | LT'
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
      "between" -> pure Between
      "eq" -> pure EQ'
      "ge" -> pure GE
      "gt" -> pure GT'
      "in" -> pure IN
      "le" -> pure LE
      "lt" -> pure LT'
      e ->
        fromTextError $
          "Failure parsing OperatorType from value: '" <> e
            <> "'. Accepted values: between, eq, ge, gt, in, le, lt"

instance ToText OperatorType where
  toText = \case
    Between -> "between"
    EQ' -> "eq"
    GE -> "ge"
    GT' -> "gt"
    IN -> "in"
    LE -> "le"
    LT' -> "lt"

instance Hashable OperatorType

instance NFData OperatorType

instance ToByteString OperatorType

instance ToQuery OperatorType

instance ToHeader OperatorType
