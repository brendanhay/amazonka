{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest where

import Network.AWS.Prelude

-- | When you use DRM with CMAF outputs, choose whether the service writes the 128-bit encryption initialization vector in the HLS and DASH manifests.
data CmafInitializationVectorInManifest
  = CIVIMExclude
  | CIVIMInclude
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

instance FromText CmafInitializationVectorInManifest where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure CIVIMExclude
      "include" -> pure CIVIMInclude
      e ->
        fromTextError $
          "Failure parsing CmafInitializationVectorInManifest from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText CmafInitializationVectorInManifest where
  toText = \case
    CIVIMExclude -> "EXCLUDE"
    CIVIMInclude -> "INCLUDE"

instance Hashable CmafInitializationVectorInManifest

instance NFData CmafInitializationVectorInManifest

instance ToByteString CmafInitializationVectorInManifest

instance ToQuery CmafInitializationVectorInManifest

instance ToHeader CmafInitializationVectorInManifest

instance ToJSON CmafInitializationVectorInManifest where
  toJSON = toJSONText

instance FromJSON CmafInitializationVectorInManifest where
  parseJSON = parseJSONText "CmafInitializationVectorInManifest"
