{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest
  ( CmafInitializationVectorInManifest
      ( CmafInitializationVectorInManifest',
        CmafInitializationVectorInManifestInclude,
        CmafInitializationVectorInManifestExclude,
        fromCmafInitializationVectorInManifest
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | When you use DRM with CMAF outputs, choose whether the service writes the 128-bit encryption initialization vector in the HLS and DASH manifests.
newtype CmafInitializationVectorInManifest = CmafInitializationVectorInManifest'
  { fromCmafInitializationVectorInManifest ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern CmafInitializationVectorInManifestInclude :: CmafInitializationVectorInManifest
pattern CmafInitializationVectorInManifestInclude = CmafInitializationVectorInManifest' "INCLUDE"

pattern CmafInitializationVectorInManifestExclude :: CmafInitializationVectorInManifest
pattern CmafInitializationVectorInManifestExclude = CmafInitializationVectorInManifest' "EXCLUDE"

{-# COMPLETE
  CmafInitializationVectorInManifestInclude,
  CmafInitializationVectorInManifestExclude,
  CmafInitializationVectorInManifest'
  #-}
