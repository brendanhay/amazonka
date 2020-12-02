{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputPreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputPreference where

import Network.AWS.Prelude

-- | Input preference when deciding which input to make active when a previously failed input has recovered.
--
-- If \"EQUAL_INPUT_PREFERENCE\", then the active input will stay active as long as it is healthy.
-- If \"PRIMARY_INPUT_PREFERRED\", then always switch back to the primary input when it is healthy.
data InputPreference
  = EqualInputPreference
  | PrimaryInputPreferred
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

instance FromText InputPreference where
  parser =
    takeLowerText >>= \case
      "equal_input_preference" -> pure EqualInputPreference
      "primary_input_preferred" -> pure PrimaryInputPreferred
      e ->
        fromTextError $
          "Failure parsing InputPreference from value: '" <> e
            <> "'. Accepted values: equal_input_preference, primary_input_preferred"

instance ToText InputPreference where
  toText = \case
    EqualInputPreference -> "EQUAL_INPUT_PREFERENCE"
    PrimaryInputPreferred -> "PRIMARY_INPUT_PREFERRED"

instance Hashable InputPreference

instance NFData InputPreference

instance ToByteString InputPreference

instance ToQuery InputPreference

instance ToHeader InputPreference

instance ToJSON InputPreference where
  toJSON = toJSONText

instance FromJSON InputPreference where
  parseJSON = parseJSONText "InputPreference"
