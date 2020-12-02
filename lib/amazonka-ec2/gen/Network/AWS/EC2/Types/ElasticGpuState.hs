{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ElasticGpuState = Attached
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

instance FromText ElasticGpuState where
  parser =
    takeLowerText >>= \case
      "attached" -> pure Attached
      e ->
        fromTextError $
          "Failure parsing ElasticGpuState from value: '" <> e
            <> "'. Accepted values: attached"

instance ToText ElasticGpuState where
  toText = \case
    Attached -> "ATTACHED"

instance Hashable ElasticGpuState

instance NFData ElasticGpuState

instance ToByteString ElasticGpuState

instance ToQuery ElasticGpuState

instance ToHeader ElasticGpuState

instance FromXML ElasticGpuState where
  parseXML = parseXMLText "ElasticGpuState"
