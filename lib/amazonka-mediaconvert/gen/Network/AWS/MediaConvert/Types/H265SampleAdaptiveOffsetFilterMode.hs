{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
  ( H265SampleAdaptiveOffsetFilterMode
      ( H265SampleAdaptiveOffsetFilterMode',
        HSAOFMAdaptive,
        HSAOFMDefault,
        HSAOFMOff
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
newtype H265SampleAdaptiveOffsetFilterMode = H265SampleAdaptiveOffsetFilterMode' Lude.Text
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

pattern HSAOFMAdaptive :: H265SampleAdaptiveOffsetFilterMode
pattern HSAOFMAdaptive = H265SampleAdaptiveOffsetFilterMode' "ADAPTIVE"

pattern HSAOFMDefault :: H265SampleAdaptiveOffsetFilterMode
pattern HSAOFMDefault = H265SampleAdaptiveOffsetFilterMode' "DEFAULT"

pattern HSAOFMOff :: H265SampleAdaptiveOffsetFilterMode
pattern HSAOFMOff = H265SampleAdaptiveOffsetFilterMode' "OFF"

{-# COMPLETE
  HSAOFMAdaptive,
  HSAOFMDefault,
  HSAOFMOff,
  H265SampleAdaptiveOffsetFilterMode'
  #-}
