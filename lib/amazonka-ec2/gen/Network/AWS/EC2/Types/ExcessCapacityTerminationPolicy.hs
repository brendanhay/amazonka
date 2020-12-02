{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExcessCapacityTerminationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExcessCapacityTerminationPolicy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ExcessCapacityTerminationPolicy
  = ECTPDefault
  | ECTPNoTermination
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

instance FromText ExcessCapacityTerminationPolicy where
  parser =
    takeLowerText >>= \case
      "default" -> pure ECTPDefault
      "notermination" -> pure ECTPNoTermination
      e ->
        fromTextError $
          "Failure parsing ExcessCapacityTerminationPolicy from value: '" <> e
            <> "'. Accepted values: default, notermination"

instance ToText ExcessCapacityTerminationPolicy where
  toText = \case
    ECTPDefault -> "default"
    ECTPNoTermination -> "noTermination"

instance Hashable ExcessCapacityTerminationPolicy

instance NFData ExcessCapacityTerminationPolicy

instance ToByteString ExcessCapacityTerminationPolicy

instance ToQuery ExcessCapacityTerminationPolicy

instance ToHeader ExcessCapacityTerminationPolicy

instance FromXML ExcessCapacityTerminationPolicy where
  parseXML = parseXMLText "ExcessCapacityTerminationPolicy"
