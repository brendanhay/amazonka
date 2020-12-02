{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafWriteDASHManifest where

import Network.AWS.Prelude

-- | When set to ENABLED, a DASH MPD manifest will be generated for this output.
data CmafWriteDASHManifest
  = CWDASHMDisabled
  | CWDASHMEnabled
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

instance FromText CmafWriteDASHManifest where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure CWDASHMDisabled
      "enabled" -> pure CWDASHMEnabled
      e ->
        fromTextError $
          "Failure parsing CmafWriteDASHManifest from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText CmafWriteDASHManifest where
  toText = \case
    CWDASHMDisabled -> "DISABLED"
    CWDASHMEnabled -> "ENABLED"

instance Hashable CmafWriteDASHManifest

instance NFData CmafWriteDASHManifest

instance ToByteString CmafWriteDASHManifest

instance ToQuery CmafWriteDASHManifest

instance ToHeader CmafWriteDASHManifest

instance ToJSON CmafWriteDASHManifest where
  toJSON = toJSONText

instance FromJSON CmafWriteDASHManifest where
  parseJSON = parseJSONText "CmafWriteDASHManifest"
