{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.MultiAZStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.MultiAZStatus where

import Network.AWS.Prelude

data MultiAZStatus
  = Disabled
  | Enabled
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

instance FromText MultiAZStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing MultiAZStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText MultiAZStatus where
  toText = \case
    Disabled -> "disabled"
    Enabled -> "enabled"

instance Hashable MultiAZStatus

instance NFData MultiAZStatus

instance ToByteString MultiAZStatus

instance ToQuery MultiAZStatus

instance ToHeader MultiAZStatus

instance FromXML MultiAZStatus where
  parseXML = parseXMLText "MultiAZStatus"
