{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AutoPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AutoPlacement where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AutoPlacement
  = APON
  | APOff
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

instance FromText AutoPlacement where
  parser =
    takeLowerText >>= \case
      "on" -> pure APON
      "off" -> pure APOff
      e ->
        fromTextError $
          "Failure parsing AutoPlacement from value: '" <> e
            <> "'. Accepted values: on, off"

instance ToText AutoPlacement where
  toText = \case
    APON -> "on"
    APOff -> "off"

instance Hashable AutoPlacement

instance NFData AutoPlacement

instance ToByteString AutoPlacement

instance ToQuery AutoPlacement

instance ToHeader AutoPlacement

instance FromXML AutoPlacement where
  parseXML = parseXMLText "AutoPlacement"
