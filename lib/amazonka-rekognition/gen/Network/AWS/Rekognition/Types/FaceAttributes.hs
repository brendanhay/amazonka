{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceAttributes where

import Network.AWS.Prelude

data FaceAttributes
  = FAAll
  | FADefault
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

instance FromText FaceAttributes where
  parser =
    takeLowerText >>= \case
      "all" -> pure FAAll
      "default" -> pure FADefault
      e ->
        fromTextError $
          "Failure parsing FaceAttributes from value: '" <> e
            <> "'. Accepted values: all, default"

instance ToText FaceAttributes where
  toText = \case
    FAAll -> "ALL"
    FADefault -> "DEFAULT"

instance Hashable FaceAttributes

instance NFData FaceAttributes

instance ToByteString FaceAttributes

instance ToQuery FaceAttributes

instance ToHeader FaceAttributes

instance ToJSON FaceAttributes where
  toJSON = toJSONText
