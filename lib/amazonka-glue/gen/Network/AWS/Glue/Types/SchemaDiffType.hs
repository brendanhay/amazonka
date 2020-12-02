{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaDiffType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaDiffType where

import Network.AWS.Prelude

data SchemaDiffType = SyntaxDiff
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

instance FromText SchemaDiffType where
  parser =
    takeLowerText >>= \case
      "syntax_diff" -> pure SyntaxDiff
      e ->
        fromTextError $
          "Failure parsing SchemaDiffType from value: '" <> e
            <> "'. Accepted values: syntax_diff"

instance ToText SchemaDiffType where
  toText = \case
    SyntaxDiff -> "SYNTAX_DIFF"

instance Hashable SchemaDiffType

instance NFData SchemaDiffType

instance ToByteString SchemaDiffType

instance ToQuery SchemaDiffType

instance ToHeader SchemaDiffType

instance ToJSON SchemaDiffType where
  toJSON = toJSONText
