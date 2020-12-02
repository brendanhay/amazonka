{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillType where

import Network.AWS.Prelude

data SkillType
  = Private
  | Public
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

instance FromText SkillType where
  parser =
    takeLowerText >>= \case
      "private" -> pure Private
      "public" -> pure Public
      e ->
        fromTextError $
          "Failure parsing SkillType from value: '" <> e
            <> "'. Accepted values: private, public"

instance ToText SkillType where
  toText = \case
    Private -> "PRIVATE"
    Public -> "PUBLIC"

instance Hashable SkillType

instance NFData SkillType

instance ToByteString SkillType

instance ToQuery SkillType

instance ToHeader SkillType

instance FromJSON SkillType where
  parseJSON = parseJSONText "SkillType"
