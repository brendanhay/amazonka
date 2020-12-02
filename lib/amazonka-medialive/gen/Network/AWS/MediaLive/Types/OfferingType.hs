{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OfferingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OfferingType where

import Network.AWS.Prelude

-- | Offering type, e.g. 'NO_UPFRONT'
data OfferingType = NoUpfront
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

instance FromText OfferingType where
  parser =
    takeLowerText >>= \case
      "no_upfront" -> pure NoUpfront
      e ->
        fromTextError $
          "Failure parsing OfferingType from value: '" <> e
            <> "'. Accepted values: no_upfront"

instance ToText OfferingType where
  toText = \case
    NoUpfront -> "NO_UPFRONT"

instance Hashable OfferingType

instance NFData OfferingType

instance ToByteString OfferingType

instance ToQuery OfferingType

instance ToHeader OfferingType

instance FromJSON OfferingType where
  parseJSON = parseJSONText "OfferingType"
