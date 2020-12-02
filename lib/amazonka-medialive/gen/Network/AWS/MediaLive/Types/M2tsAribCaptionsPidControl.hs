{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsAribCaptionsPidControl where

import Network.AWS.Prelude

-- | M2ts Arib Captions Pid Control
data M2tsAribCaptionsPidControl
  = MACPCAuto
  | MACPCUseConfigured
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

instance FromText M2tsAribCaptionsPidControl where
  parser =
    takeLowerText >>= \case
      "auto" -> pure MACPCAuto
      "use_configured" -> pure MACPCUseConfigured
      e ->
        fromTextError $
          "Failure parsing M2tsAribCaptionsPidControl from value: '" <> e
            <> "'. Accepted values: auto, use_configured"

instance ToText M2tsAribCaptionsPidControl where
  toText = \case
    MACPCAuto -> "AUTO"
    MACPCUseConfigured -> "USE_CONFIGURED"

instance Hashable M2tsAribCaptionsPidControl

instance NFData M2tsAribCaptionsPidControl

instance ToByteString M2tsAribCaptionsPidControl

instance ToQuery M2tsAribCaptionsPidControl

instance ToHeader M2tsAribCaptionsPidControl

instance ToJSON M2tsAribCaptionsPidControl where
  toJSON = toJSONText

instance FromJSON M2tsAribCaptionsPidControl where
  parseJSON = parseJSONText "M2tsAribCaptionsPidControl"
