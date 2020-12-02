{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest where

import Network.AWS.Prelude

-- | The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
data HlsInitializationVectorInManifest
  = HIVIMExclude
  | HIVIMInclude
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

instance FromText HlsInitializationVectorInManifest where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure HIVIMExclude
      "include" -> pure HIVIMInclude
      e ->
        fromTextError $
          "Failure parsing HlsInitializationVectorInManifest from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText HlsInitializationVectorInManifest where
  toText = \case
    HIVIMExclude -> "EXCLUDE"
    HIVIMInclude -> "INCLUDE"

instance Hashable HlsInitializationVectorInManifest

instance NFData HlsInitializationVectorInManifest

instance ToByteString HlsInitializationVectorInManifest

instance ToQuery HlsInitializationVectorInManifest

instance ToHeader HlsInitializationVectorInManifest

instance ToJSON HlsInitializationVectorInManifest where
  toJSON = toJSONText

instance FromJSON HlsInitializationVectorInManifest where
  parseJSON = parseJSONText "HlsInitializationVectorInManifest"
