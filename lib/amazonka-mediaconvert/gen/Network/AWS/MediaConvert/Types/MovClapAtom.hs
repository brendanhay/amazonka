{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovClapAtom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovClapAtom where

import Network.AWS.Prelude

-- | When enabled, include 'clap' atom if appropriate for the video output settings.
data MovClapAtom
  = MExclude
  | MInclude
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

instance FromText MovClapAtom where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure MExclude
      "include" -> pure MInclude
      e ->
        fromTextError $
          "Failure parsing MovClapAtom from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText MovClapAtom where
  toText = \case
    MExclude -> "EXCLUDE"
    MInclude -> "INCLUDE"

instance Hashable MovClapAtom

instance NFData MovClapAtom

instance ToByteString MovClapAtom

instance ToQuery MovClapAtom

instance ToHeader MovClapAtom

instance ToJSON MovClapAtom where
  toJSON = toJSONText

instance FromJSON MovClapAtom where
  parseJSON = parseJSONText "MovClapAtom"
