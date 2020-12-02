{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInTeletextGridControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInTeletextGridControl where

import Network.AWS.Prelude

-- | Burn In Teletext Grid Control
data BurnInTeletextGridControl
  = BITGCFixed
  | BITGCScaled
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

instance FromText BurnInTeletextGridControl where
  parser =
    takeLowerText >>= \case
      "fixed" -> pure BITGCFixed
      "scaled" -> pure BITGCScaled
      e ->
        fromTextError $
          "Failure parsing BurnInTeletextGridControl from value: '" <> e
            <> "'. Accepted values: fixed, scaled"

instance ToText BurnInTeletextGridControl where
  toText = \case
    BITGCFixed -> "FIXED"
    BITGCScaled -> "SCALED"

instance Hashable BurnInTeletextGridControl

instance NFData BurnInTeletextGridControl

instance ToByteString BurnInTeletextGridControl

instance ToQuery BurnInTeletextGridControl

instance ToHeader BurnInTeletextGridControl

instance ToJSON BurnInTeletextGridControl where
  toJSON = toJSONText

instance FromJSON BurnInTeletextGridControl where
  parseJSON = parseJSONText "BurnInTeletextGridControl"
