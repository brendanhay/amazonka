{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ContainerFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ContainerFormat where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ContainerFormat = Ova
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

instance FromText ContainerFormat where
  parser =
    takeLowerText >>= \case
      "ova" -> pure Ova
      e ->
        fromTextError $
          "Failure parsing ContainerFormat from value: '" <> e
            <> "'. Accepted values: ova"

instance ToText ContainerFormat where
  toText = \case
    Ova -> "ova"

instance Hashable ContainerFormat

instance NFData ContainerFormat

instance ToByteString ContainerFormat

instance ToQuery ContainerFormat

instance ToHeader ContainerFormat

instance FromXML ContainerFormat where
  parseXML = parseXMLText "ContainerFormat"
