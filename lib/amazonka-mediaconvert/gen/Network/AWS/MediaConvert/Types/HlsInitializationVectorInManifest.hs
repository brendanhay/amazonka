-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
  ( HlsInitializationVectorInManifest
      ( HlsInitializationVectorInManifest',
        HIVIMExclude,
        HIVIMInclude
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
newtype HlsInitializationVectorInManifest = HlsInitializationVectorInManifest' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern HIVIMExclude :: HlsInitializationVectorInManifest
pattern HIVIMExclude = HlsInitializationVectorInManifest' "EXCLUDE"

pattern HIVIMInclude :: HlsInitializationVectorInManifest
pattern HIVIMInclude = HlsInitializationVectorInManifest' "INCLUDE"

{-# COMPLETE
  HIVIMExclude,
  HIVIMInclude,
  HlsInitializationVectorInManifest'
  #-}
