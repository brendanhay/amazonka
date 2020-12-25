{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType
  ( NielsenSourceWatermarkStatusType
      ( NielsenSourceWatermarkStatusType',
        NielsenSourceWatermarkStatusTypeClean,
        NielsenSourceWatermarkStatusTypeWatermarked,
        fromNielsenSourceWatermarkStatusType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Required. Specify whether your source content already contains Nielsen non-linear watermarks. When you set this value to Watermarked (WATERMARKED), the service fails the job. Nielsen requires that you add non-linear watermarking to only clean content that doesn't already  have non-linear Nielsen watermarks.
newtype NielsenSourceWatermarkStatusType = NielsenSourceWatermarkStatusType'
  { fromNielsenSourceWatermarkStatusType ::
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

pattern NielsenSourceWatermarkStatusTypeClean :: NielsenSourceWatermarkStatusType
pattern NielsenSourceWatermarkStatusTypeClean = NielsenSourceWatermarkStatusType' "CLEAN"

pattern NielsenSourceWatermarkStatusTypeWatermarked :: NielsenSourceWatermarkStatusType
pattern NielsenSourceWatermarkStatusTypeWatermarked = NielsenSourceWatermarkStatusType' "WATERMARKED"

{-# COMPLETE
  NielsenSourceWatermarkStatusTypeClean,
  NielsenSourceWatermarkStatusTypeWatermarked,
  NielsenSourceWatermarkStatusType'
  #-}
