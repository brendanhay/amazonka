{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceLifecycle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceLifecycle where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceLifecycle
  = ILOnDemand
  | ILSpot
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

instance FromText InstanceLifecycle where
  parser =
    takeLowerText >>= \case
      "on-demand" -> pure ILOnDemand
      "spot" -> pure ILSpot
      e ->
        fromTextError $
          "Failure parsing InstanceLifecycle from value: '" <> e
            <> "'. Accepted values: on-demand, spot"

instance ToText InstanceLifecycle where
  toText = \case
    ILOnDemand -> "on-demand"
    ILSpot -> "spot"

instance Hashable InstanceLifecycle

instance NFData InstanceLifecycle

instance ToByteString InstanceLifecycle

instance ToQuery InstanceLifecycle

instance ToHeader InstanceLifecycle

instance FromXML InstanceLifecycle where
  parseXML = parseXMLText "InstanceLifecycle"
