{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
  ( HlsInitializationVectorInManifest
    ( HlsInitializationVectorInManifest'
    , HlsInitializationVectorInManifestInclude
    , HlsInitializationVectorInManifestExclude
    , fromHlsInitializationVectorInManifest
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
newtype HlsInitializationVectorInManifest = HlsInitializationVectorInManifest'{fromHlsInitializationVectorInManifest
                                                                               :: Core.Text}
                                              deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                              Core.Show, Core.Generic)
                                              deriving newtype (Core.IsString, Core.Hashable,
                                                                Core.NFData, Core.ToJSONKey,
                                                                Core.FromJSONKey, Core.ToJSON,
                                                                Core.FromJSON, Core.ToXML,
                                                                Core.FromXML, Core.ToText,
                                                                Core.FromText, Core.ToByteString,
                                                                Core.ToQuery, Core.ToHeader)

pattern HlsInitializationVectorInManifestInclude :: HlsInitializationVectorInManifest
pattern HlsInitializationVectorInManifestInclude = HlsInitializationVectorInManifest' "INCLUDE"

pattern HlsInitializationVectorInManifestExclude :: HlsInitializationVectorInManifest
pattern HlsInitializationVectorInManifestExclude = HlsInitializationVectorInManifest' "EXCLUDE"

{-# COMPLETE 
  HlsInitializationVectorInManifestInclude,

  HlsInitializationVectorInManifestExclude,
  HlsInitializationVectorInManifest'
  #-}
