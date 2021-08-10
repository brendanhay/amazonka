{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265WriteMp4PackagingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265WriteMp4PackagingType
  ( H265WriteMp4PackagingType
      ( ..,
        H265WriteMp4PackagingType_HEV1,
        H265WriteMp4PackagingType_HVC1
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | If the location of parameter set NAL units doesn\'t matter in your
-- workflow, ignore this setting. Use this setting only with CMAF or DASH
-- outputs, or with standalone file outputs in an MPEG-4 container (MP4
-- outputs). Choose HVC1 to mark your output as HVC1. This makes your
-- output compliant with the following specification: ISO IECJTC1 SC29
-- N13798 Text ISO\/IEC FDIS 14496-15 3rd Edition. For these outputs, the
-- service stores parameter set NAL units in the sample headers but not in
-- the samples directly. For MP4 outputs, when you choose HVC1, your output
-- video might not work properly with some downstream systems and video
-- players. The service defaults to marking your output as HEV1. For these
-- outputs, the service writes parameter set NAL units directly into the
-- samples.
newtype H265WriteMp4PackagingType = H265WriteMp4PackagingType'
  { fromH265WriteMp4PackagingType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern H265WriteMp4PackagingType_HEV1 :: H265WriteMp4PackagingType
pattern H265WriteMp4PackagingType_HEV1 = H265WriteMp4PackagingType' "HEV1"

pattern H265WriteMp4PackagingType_HVC1 :: H265WriteMp4PackagingType
pattern H265WriteMp4PackagingType_HVC1 = H265WriteMp4PackagingType' "HVC1"

{-# COMPLETE
  H265WriteMp4PackagingType_HEV1,
  H265WriteMp4PackagingType_HVC1,
  H265WriteMp4PackagingType'
  #-}
