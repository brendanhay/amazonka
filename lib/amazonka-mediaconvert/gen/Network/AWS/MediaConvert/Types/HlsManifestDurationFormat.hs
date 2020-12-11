-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsManifestDurationFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsManifestDurationFormat
  ( HlsManifestDurationFormat
      ( HlsManifestDurationFormat',
        HMDFFloatingPoint,
        HMDFInteger
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Indicates whether the output manifest should use floating point values for segment duration.
newtype HlsManifestDurationFormat = HlsManifestDurationFormat' Lude.Text
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

pattern HMDFFloatingPoint :: HlsManifestDurationFormat
pattern HMDFFloatingPoint = HlsManifestDurationFormat' "FLOATING_POINT"

pattern HMDFInteger :: HlsManifestDurationFormat
pattern HMDFInteger = HlsManifestDurationFormat' "INTEGER"

{-# COMPLETE
  HMDFFloatingPoint,
  HMDFInteger,
  HlsManifestDurationFormat'
  #-}
