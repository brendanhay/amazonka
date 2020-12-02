{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TtmlDestinationStyleControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TtmlDestinationStyleControl where

import Network.AWS.Prelude

-- | Ttml Destination Style Control
data TtmlDestinationStyleControl
  = TDSCPassthrough
  | TDSCUseConfigured
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

instance FromText TtmlDestinationStyleControl where
  parser =
    takeLowerText >>= \case
      "passthrough" -> pure TDSCPassthrough
      "use_configured" -> pure TDSCUseConfigured
      e ->
        fromTextError $
          "Failure parsing TtmlDestinationStyleControl from value: '" <> e
            <> "'. Accepted values: passthrough, use_configured"

instance ToText TtmlDestinationStyleControl where
  toText = \case
    TDSCPassthrough -> "PASSTHROUGH"
    TDSCUseConfigured -> "USE_CONFIGURED"

instance Hashable TtmlDestinationStyleControl

instance NFData TtmlDestinationStyleControl

instance ToByteString TtmlDestinationStyleControl

instance ToQuery TtmlDestinationStyleControl

instance ToHeader TtmlDestinationStyleControl

instance ToJSON TtmlDestinationStyleControl where
  toJSON = toJSONText

instance FromJSON TtmlDestinationStyleControl where
  parseJSON = parseJSONText "TtmlDestinationStyleControl"
