{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Mode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Mode where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data Mode
  = HighPerformance
  | Standard
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

instance FromText Mode where
  parser =
    takeLowerText >>= \case
      "high-performance" -> pure HighPerformance
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing Mode from value: '" <> e
            <> "'. Accepted values: high-performance, standard"

instance ToText Mode where
  toText = \case
    HighPerformance -> "high-performance"
    Standard -> "standard"

instance Hashable Mode

instance NFData Mode

instance ToByteString Mode

instance ToQuery Mode

instance ToHeader Mode

instance FromXML Mode where
  parseXML = parseXMLText "Mode"
