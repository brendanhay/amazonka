{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
  ( CmafWriteDASHManifest
    ( CmafWriteDASHManifest'
    , CmafWriteDASHManifestDisabled
    , CmafWriteDASHManifestEnabled
    , fromCmafWriteDASHManifest
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When set to ENABLED, a DASH MPD manifest will be generated for this output.
newtype CmafWriteDASHManifest = CmafWriteDASHManifest'{fromCmafWriteDASHManifest
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern CmafWriteDASHManifestDisabled :: CmafWriteDASHManifest
pattern CmafWriteDASHManifestDisabled = CmafWriteDASHManifest' "DISABLED"

pattern CmafWriteDASHManifestEnabled :: CmafWriteDASHManifest
pattern CmafWriteDASHManifestEnabled = CmafWriteDASHManifest' "ENABLED"

{-# COMPLETE 
  CmafWriteDASHManifestDisabled,

  CmafWriteDASHManifestEnabled,
  CmafWriteDASHManifest'
  #-}
