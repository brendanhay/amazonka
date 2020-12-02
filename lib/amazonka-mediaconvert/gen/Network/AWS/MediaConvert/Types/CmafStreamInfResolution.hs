{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafStreamInfResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafStreamInfResolution where

import Network.AWS.Prelude

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
data CmafStreamInfResolution
  = CSIRExclude
  | CSIRInclude
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

instance FromText CmafStreamInfResolution where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure CSIRExclude
      "include" -> pure CSIRInclude
      e ->
        fromTextError $
          "Failure parsing CmafStreamInfResolution from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText CmafStreamInfResolution where
  toText = \case
    CSIRExclude -> "EXCLUDE"
    CSIRInclude -> "INCLUDE"

instance Hashable CmafStreamInfResolution

instance NFData CmafStreamInfResolution

instance ToByteString CmafStreamInfResolution

instance ToQuery CmafStreamInfResolution

instance ToHeader CmafStreamInfResolution

instance ToJSON CmafStreamInfResolution where
  toJSON = toJSONText

instance FromJSON CmafStreamInfResolution where
  parseJSON = parseJSONText "CmafStreamInfResolution"
