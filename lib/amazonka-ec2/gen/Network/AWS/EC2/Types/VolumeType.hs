{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeType
  = GP2
  | IO1
  | IO2
  | SC1
  | ST1
  | Standard
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

instance FromText VolumeType where
  parser =
    takeLowerText >>= \case
      "gp2" -> pure GP2
      "io1" -> pure IO1
      "io2" -> pure IO2
      "sc1" -> pure SC1
      "st1" -> pure ST1
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing VolumeType from value: '" <> e
            <> "'. Accepted values: gp2, io1, io2, sc1, st1, standard"

instance ToText VolumeType where
  toText = \case
    GP2 -> "gp2"
    IO1 -> "io1"
    IO2 -> "io2"
    SC1 -> "sc1"
    ST1 -> "st1"
    Standard -> "standard"

instance Hashable VolumeType

instance NFData VolumeType

instance ToByteString VolumeType

instance ToQuery VolumeType

instance ToHeader VolumeType

instance FromXML VolumeType where
  parseXML = parseXMLText "VolumeType"
