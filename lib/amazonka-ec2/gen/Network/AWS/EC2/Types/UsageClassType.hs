{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UsageClassType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UsageClassType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data UsageClassType
  = UCTOnDemand
  | UCTSpot
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

instance FromText UsageClassType where
  parser =
    takeLowerText >>= \case
      "on-demand" -> pure UCTOnDemand
      "spot" -> pure UCTSpot
      e ->
        fromTextError $
          "Failure parsing UsageClassType from value: '" <> e
            <> "'. Accepted values: on-demand, spot"

instance ToText UsageClassType where
  toText = \case
    UCTOnDemand -> "on-demand"
    UCTSpot -> "spot"

instance Hashable UsageClassType

instance NFData UsageClassType

instance ToByteString UsageClassType

instance ToQuery UsageClassType

instance ToHeader UsageClassType

instance FromXML UsageClassType where
  parseXML = parseXMLText "UsageClassType"
