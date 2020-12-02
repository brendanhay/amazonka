{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMatchCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMatchCriteria where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceMatchCriteria
  = IMCOpen
  | IMCTargeted
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

instance FromText InstanceMatchCriteria where
  parser =
    takeLowerText >>= \case
      "open" -> pure IMCOpen
      "targeted" -> pure IMCTargeted
      e ->
        fromTextError $
          "Failure parsing InstanceMatchCriteria from value: '" <> e
            <> "'. Accepted values: open, targeted"

instance ToText InstanceMatchCriteria where
  toText = \case
    IMCOpen -> "open"
    IMCTargeted -> "targeted"

instance Hashable InstanceMatchCriteria

instance NFData InstanceMatchCriteria

instance ToByteString InstanceMatchCriteria

instance ToQuery InstanceMatchCriteria

instance ToHeader InstanceMatchCriteria

instance FromXML InstanceMatchCriteria where
  parseXML = parseXMLText "InstanceMatchCriteria"
