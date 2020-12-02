{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PortAccessType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PortAccessType where

import Network.AWS.Prelude

data PortAccessType
  = Private
  | Public
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

instance FromText PortAccessType where
  parser =
    takeLowerText >>= \case
      "private" -> pure Private
      "public" -> pure Public
      e ->
        fromTextError $
          "Failure parsing PortAccessType from value: '" <> e
            <> "'. Accepted values: private, public"

instance ToText PortAccessType where
  toText = \case
    Private -> "Private"
    Public -> "Public"

instance Hashable PortAccessType

instance NFData PortAccessType

instance ToByteString PortAccessType

instance ToQuery PortAccessType

instance ToHeader PortAccessType

instance FromJSON PortAccessType where
  parseJSON = parseJSONText "PortAccessType"
