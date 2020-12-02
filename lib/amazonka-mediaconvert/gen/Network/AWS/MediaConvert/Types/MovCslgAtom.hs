{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovCslgAtom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovCslgAtom where

import Network.AWS.Prelude

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
data MovCslgAtom
  = MCAExclude
  | MCAInclude
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

instance FromText MovCslgAtom where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure MCAExclude
      "include" -> pure MCAInclude
      e ->
        fromTextError $
          "Failure parsing MovCslgAtom from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText MovCslgAtom where
  toText = \case
    MCAExclude -> "EXCLUDE"
    MCAInclude -> "INCLUDE"

instance Hashable MovCslgAtom

instance NFData MovCslgAtom

instance ToByteString MovCslgAtom

instance ToQuery MovCslgAtom

instance ToHeader MovCslgAtom

instance ToJSON MovCslgAtom where
  toJSON = toJSONText

instance FromJSON MovCslgAtom where
  parseJSON = parseJSONText "MovCslgAtom"
