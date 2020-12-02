{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.StatementType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.StatementType where

import Network.AWS.Prelude

data StatementType
  = Ddl
  | Dml
  | Utility
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

instance FromText StatementType where
  parser =
    takeLowerText >>= \case
      "ddl" -> pure Ddl
      "dml" -> pure Dml
      "utility" -> pure Utility
      e ->
        fromTextError $
          "Failure parsing StatementType from value: '" <> e
            <> "'. Accepted values: ddl, dml, utility"

instance ToText StatementType where
  toText = \case
    Ddl -> "DDL"
    Dml -> "DML"
    Utility -> "UTILITY"

instance Hashable StatementType

instance NFData StatementType

instance ToByteString StatementType

instance ToQuery StatementType

instance ToHeader StatementType

instance FromJSON StatementType where
  parseJSON = parseJSONText "StatementType"
