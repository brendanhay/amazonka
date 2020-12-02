{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceLifecycleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceLifecycleType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceLifecycleType
  = ILTScheduled
  | ILTSpot
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

instance FromText InstanceLifecycleType where
  parser =
    takeLowerText >>= \case
      "scheduled" -> pure ILTScheduled
      "spot" -> pure ILTSpot
      e ->
        fromTextError $
          "Failure parsing InstanceLifecycleType from value: '" <> e
            <> "'. Accepted values: scheduled, spot"

instance ToText InstanceLifecycleType where
  toText = \case
    ILTScheduled -> "scheduled"
    ILTSpot -> "spot"

instance Hashable InstanceLifecycleType

instance NFData InstanceLifecycleType

instance ToByteString InstanceLifecycleType

instance ToQuery InstanceLifecycleType

instance ToHeader InstanceLifecycleType

instance FromXML InstanceLifecycleType where
  parseXML = parseXMLText "InstanceLifecycleType"
