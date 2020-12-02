{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteHLSManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafWriteHLSManifest where

import Network.AWS.Prelude

-- | When set to ENABLED, an Apple HLS manifest will be generated for this output.
data CmafWriteHLSManifest
  = CWHLSMDisabled
  | CWHLSMEnabled
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

instance FromText CmafWriteHLSManifest where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure CWHLSMDisabled
      "enabled" -> pure CWHLSMEnabled
      e ->
        fromTextError $
          "Failure parsing CmafWriteHLSManifest from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText CmafWriteHLSManifest where
  toText = \case
    CWHLSMDisabled -> "DISABLED"
    CWHLSMEnabled -> "ENABLED"

instance Hashable CmafWriteHLSManifest

instance NFData CmafWriteHLSManifest

instance ToByteString CmafWriteHLSManifest

instance ToQuery CmafWriteHLSManifest

instance ToHeader CmafWriteHLSManifest

instance ToJSON CmafWriteHLSManifest where
  toJSON = toJSONText

instance FromJSON CmafWriteHLSManifest where
  parseJSON = parseJSONText "CmafWriteHLSManifest"
