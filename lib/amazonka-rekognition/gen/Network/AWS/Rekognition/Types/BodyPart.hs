{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.BodyPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.BodyPart where

import Network.AWS.Prelude

data BodyPart
  = Face
  | Head
  | LeftHand
  | RightHand
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

instance FromText BodyPart where
  parser =
    takeLowerText >>= \case
      "face" -> pure Face
      "head" -> pure Head
      "left_hand" -> pure LeftHand
      "right_hand" -> pure RightHand
      e ->
        fromTextError $
          "Failure parsing BodyPart from value: '" <> e
            <> "'. Accepted values: face, head, left_hand, right_hand"

instance ToText BodyPart where
  toText = \case
    Face -> "FACE"
    Head -> "HEAD"
    LeftHand -> "LEFT_HAND"
    RightHand -> "RIGHT_HAND"

instance Hashable BodyPart

instance NFData BodyPart

instance ToByteString BodyPart

instance ToQuery BodyPart

instance ToHeader BodyPart

instance FromJSON BodyPart where
  parseJSON = parseJSONText "BodyPart"
