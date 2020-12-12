{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
  ( CmafWriteDASHManifest
      ( CmafWriteDASHManifest',
        CWDASHMDisabled,
        CWDASHMEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When set to ENABLED, a DASH MPD manifest will be generated for this output.
newtype CmafWriteDASHManifest = CmafWriteDASHManifest' Lude.Text
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

pattern CWDASHMDisabled :: CmafWriteDASHManifest
pattern CWDASHMDisabled = CmafWriteDASHManifest' "DISABLED"

pattern CWDASHMEnabled :: CmafWriteDASHManifest
pattern CWDASHMEnabled = CmafWriteDASHManifest' "ENABLED"

{-# COMPLETE
  CWDASHMDisabled,
  CWDASHMEnabled,
  CmafWriteDASHManifest'
  #-}
