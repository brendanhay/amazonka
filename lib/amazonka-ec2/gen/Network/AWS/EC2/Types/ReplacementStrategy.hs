{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReplacementStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReplacementStrategy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ReplacementStrategy = RSLaunch
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

instance FromText ReplacementStrategy where
  parser =
    takeLowerText >>= \case
      "launch" -> pure RSLaunch
      e ->
        fromTextError $
          "Failure parsing ReplacementStrategy from value: '" <> e
            <> "'. Accepted values: launch"

instance ToText ReplacementStrategy where
  toText = \case
    RSLaunch -> "launch"

instance Hashable ReplacementStrategy

instance NFData ReplacementStrategy

instance ToByteString ReplacementStrategy

instance ToQuery ReplacementStrategy

instance ToHeader ReplacementStrategy

instance FromXML ReplacementStrategy where
  parseXML = parseXMLText "ReplacementStrategy"
