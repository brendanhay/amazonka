{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AllowsMultipleInstanceTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AllowsMultipleInstanceTypes where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AllowsMultipleInstanceTypes
  = ON
  | Off
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

instance FromText AllowsMultipleInstanceTypes where
  parser =
    takeLowerText >>= \case
      "on" -> pure ON
      "off" -> pure Off
      e ->
        fromTextError $
          "Failure parsing AllowsMultipleInstanceTypes from value: '" <> e
            <> "'. Accepted values: on, off"

instance ToText AllowsMultipleInstanceTypes where
  toText = \case
    ON -> "on"
    Off -> "off"

instance Hashable AllowsMultipleInstanceTypes

instance NFData AllowsMultipleInstanceTypes

instance ToByteString AllowsMultipleInstanceTypes

instance ToQuery AllowsMultipleInstanceTypes

instance ToHeader AllowsMultipleInstanceTypes

instance FromXML AllowsMultipleInstanceTypes where
  parseXML = parseXMLText "AllowsMultipleInstanceTypes"
