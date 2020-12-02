{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostRecovery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostRecovery where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data HostRecovery
  = HRON
  | HROff
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

instance FromText HostRecovery where
  parser =
    takeLowerText >>= \case
      "on" -> pure HRON
      "off" -> pure HROff
      e ->
        fromTextError $
          "Failure parsing HostRecovery from value: '" <> e
            <> "'. Accepted values: on, off"

instance ToText HostRecovery where
  toText = \case
    HRON -> "on"
    HROff -> "off"

instance Hashable HostRecovery

instance NFData HostRecovery

instance ToByteString HostRecovery

instance ToQuery HostRecovery

instance ToHeader HostRecovery

instance FromXML HostRecovery where
  parseXML = parseXMLText "HostRecovery"
