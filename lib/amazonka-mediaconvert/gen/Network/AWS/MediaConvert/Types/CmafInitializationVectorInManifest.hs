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
        Include,
        Exclude
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When you use DRM with CMAF outputs, choose whether the service writes the 128-bit encryption initialization vector in the HLS and DASH manifests.
newtype CmafInitializationVectorInManifest = CmafInitializationVectorInManifest' Lude.Text
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

pattern Include :: CmafInitializationVectorInManifest
pattern Include = CmafInitializationVectorInManifest' "INCLUDE"

pattern Exclude :: CmafInitializationVectorInManifest
pattern Exclude = CmafInitializationVectorInManifest' "EXCLUDE"

{-# COMPLETE
  Include,
  Exclude,
  CmafInitializationVectorInManifest'
  #-}
