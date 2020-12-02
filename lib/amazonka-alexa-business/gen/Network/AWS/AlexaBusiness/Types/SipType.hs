{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SipType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SipType where

import Network.AWS.Prelude

data SipType = Work
  deriving (Eq, Ord, Show, Enum, Bounded, Data, Typeable, Generic)

instance FromText SipType where
  parser =
    takeLowerText >>= \case
      "work" -> pure Work
      e ->
        fromTextError $
          "Failure parsing SipType from value: '" <> e
            <> "'. Accepted values: work"

instance ToText SipType where
  toText = \case
    Work -> "WORK"

instance Hashable SipType

instance NFData SipType

instance ToByteString SipType

instance ToQuery SipType

instance ToHeader SipType

instance ToJSON SipType where
  toJSON = toJSONText

instance FromJSON SipType where
  parseJSON = parseJSONText "SipType"
