{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AutomaticFailoverStatus where

import Network.AWS.Prelude

data AutomaticFailoverStatus
  = AFSDisabled
  | AFSDisabling
  | AFSEnabled
  | AFSEnabling
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

instance FromText AutomaticFailoverStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure AFSDisabled
      "disabling" -> pure AFSDisabling
      "enabled" -> pure AFSEnabled
      "enabling" -> pure AFSEnabling
      e ->
        fromTextError $
          "Failure parsing AutomaticFailoverStatus from value: '" <> e
            <> "'. Accepted values: disabled, disabling, enabled, enabling"

instance ToText AutomaticFailoverStatus where
  toText = \case
    AFSDisabled -> "disabled"
    AFSDisabling -> "disabling"
    AFSEnabled -> "enabled"
    AFSEnabling -> "enabling"

instance Hashable AutomaticFailoverStatus

instance NFData AutomaticFailoverStatus

instance ToByteString AutomaticFailoverStatus

instance ToQuery AutomaticFailoverStatus

instance ToHeader AutomaticFailoverStatus

instance FromXML AutomaticFailoverStatus where
  parseXML = parseXMLText "AutomaticFailoverStatus"
