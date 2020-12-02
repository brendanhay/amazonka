{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EbuTtDDestinationStyleControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EbuTtDDestinationStyleControl where

import Network.AWS.Prelude

-- | Ebu Tt DDestination Style Control
data EbuTtDDestinationStyleControl
  = ETDDSCExclude
  | ETDDSCInclude
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

instance FromText EbuTtDDestinationStyleControl where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure ETDDSCExclude
      "include" -> pure ETDDSCInclude
      e ->
        fromTextError $
          "Failure parsing EbuTtDDestinationStyleControl from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText EbuTtDDestinationStyleControl where
  toText = \case
    ETDDSCExclude -> "EXCLUDE"
    ETDDSCInclude -> "INCLUDE"

instance Hashable EbuTtDDestinationStyleControl

instance NFData EbuTtDDestinationStyleControl

instance ToByteString EbuTtDDestinationStyleControl

instance ToQuery EbuTtDDestinationStyleControl

instance ToHeader EbuTtDDestinationStyleControl

instance ToJSON EbuTtDDestinationStyleControl where
  toJSON = toJSONText

instance FromJSON EbuTtDDestinationStyleControl where
  parseJSON = parseJSONText "EbuTtDDestinationStyleControl"
