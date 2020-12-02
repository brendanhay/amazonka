{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StatusName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StatusName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data StatusName = Reachability
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

instance FromText StatusName where
  parser =
    takeLowerText >>= \case
      "reachability" -> pure Reachability
      e ->
        fromTextError $
          "Failure parsing StatusName from value: '" <> e
            <> "'. Accepted values: reachability"

instance ToText StatusName where
  toText = \case
    Reachability -> "reachability"

instance Hashable StatusName

instance NFData StatusName

instance ToByteString StatusName

instance ToQuery StatusName

instance ToHeader StatusName

instance FromXML StatusName where
  parseXML = parseXMLText "StatusName"
