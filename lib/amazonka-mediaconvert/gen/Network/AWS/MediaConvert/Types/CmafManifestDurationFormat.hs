{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafManifestDurationFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafManifestDurationFormat
  ( CmafManifestDurationFormat
      ( CmafManifestDurationFormat',
        FloatingPoint,
        Integer
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Indicates whether the output manifest should use floating point values for segment duration.
newtype CmafManifestDurationFormat = CmafManifestDurationFormat' Lude.Text
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

pattern FloatingPoint :: CmafManifestDurationFormat
pattern FloatingPoint = CmafManifestDurationFormat' "FLOATING_POINT"

pattern Integer :: CmafManifestDurationFormat
pattern Integer = CmafManifestDurationFormat' "INTEGER"

{-# COMPLETE
  FloatingPoint,
  Integer,
  CmafManifestDurationFormat'
  #-}
