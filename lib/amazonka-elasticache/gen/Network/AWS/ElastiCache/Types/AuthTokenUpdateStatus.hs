{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus where

import Network.AWS.Prelude

data AuthTokenUpdateStatus
  = Rotating
  | Setting
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

instance FromText AuthTokenUpdateStatus where
  parser =
    takeLowerText >>= \case
      "rotating" -> pure Rotating
      "setting" -> pure Setting
      e ->
        fromTextError $
          "Failure parsing AuthTokenUpdateStatus from value: '" <> e
            <> "'. Accepted values: rotating, setting"

instance ToText AuthTokenUpdateStatus where
  toText = \case
    Rotating -> "ROTATING"
    Setting -> "SETTING"

instance Hashable AuthTokenUpdateStatus

instance NFData AuthTokenUpdateStatus

instance ToByteString AuthTokenUpdateStatus

instance ToQuery AuthTokenUpdateStatus

instance ToHeader AuthTokenUpdateStatus

instance FromXML AuthTokenUpdateStatus where
  parseXML = parseXMLText "AuthTokenUpdateStatus"
