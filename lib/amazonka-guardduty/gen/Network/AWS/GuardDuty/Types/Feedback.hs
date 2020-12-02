{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Feedback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Feedback where

import Network.AWS.Prelude

data Feedback
  = NotUseful
  | Useful
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

instance FromText Feedback where
  parser =
    takeLowerText >>= \case
      "not_useful" -> pure NotUseful
      "useful" -> pure Useful
      e ->
        fromTextError $
          "Failure parsing Feedback from value: '" <> e
            <> "'. Accepted values: not_useful, useful"

instance ToText Feedback where
  toText = \case
    NotUseful -> "NOT_USEFUL"
    Useful -> "USEFUL"

instance Hashable Feedback

instance NFData Feedback

instance ToByteString Feedback

instance ToQuery Feedback

instance ToHeader Feedback

instance ToJSON Feedback where
  toJSON = toJSONText
