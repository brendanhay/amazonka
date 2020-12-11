-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteHLSManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafWriteHLSManifest
  ( CmafWriteHLSManifest
      ( CmafWriteHLSManifest',
        CWHLSMDisabled,
        CWHLSMEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When set to ENABLED, an Apple HLS manifest will be generated for this output.
newtype CmafWriteHLSManifest = CmafWriteHLSManifest' Lude.Text
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

pattern CWHLSMDisabled :: CmafWriteHLSManifest
pattern CWHLSMDisabled = CmafWriteHLSManifest' "DISABLED"

pattern CWHLSMEnabled :: CmafWriteHLSManifest
pattern CWHLSMEnabled = CmafWriteHLSManifest' "ENABLED"

{-# COMPLETE
  CWHLSMDisabled,
  CWHLSMEnabled,
  CmafWriteHLSManifest'
  #-}
