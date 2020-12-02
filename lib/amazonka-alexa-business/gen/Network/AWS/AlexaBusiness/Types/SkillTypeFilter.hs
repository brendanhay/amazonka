{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillTypeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillTypeFilter where

import Network.AWS.Prelude

data SkillTypeFilter
  = STFAll
  | STFPrivate
  | STFPublic
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

instance FromText SkillTypeFilter where
  parser =
    takeLowerText >>= \case
      "all" -> pure STFAll
      "private" -> pure STFPrivate
      "public" -> pure STFPublic
      e ->
        fromTextError $
          "Failure parsing SkillTypeFilter from value: '" <> e
            <> "'. Accepted values: all, private, public"

instance ToText SkillTypeFilter where
  toText = \case
    STFAll -> "ALL"
    STFPrivate -> "PRIVATE"
    STFPublic -> "PUBLIC"

instance Hashable SkillTypeFilter

instance NFData SkillTypeFilter

instance ToByteString SkillTypeFilter

instance ToQuery SkillTypeFilter

instance ToHeader SkillTypeFilter

instance ToJSON SkillTypeFilter where
  toJSON = toJSONText
