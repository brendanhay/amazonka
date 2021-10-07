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
-- Module      : Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenSourceWatermarkStatusType
  ( NielsenSourceWatermarkStatusType
      ( ..,
        NielsenSourceWatermarkStatusType_CLEAN,
        NielsenSourceWatermarkStatusType_WATERMARKED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Required. Specify whether your source content already contains Nielsen
-- non-linear watermarks. When you set this value to Watermarked
-- (WATERMARKED), the service fails the job. Nielsen requires that you add
-- non-linear watermarking to only clean content that doesn\'t already have
-- non-linear Nielsen watermarks.
newtype NielsenSourceWatermarkStatusType = NielsenSourceWatermarkStatusType'
  { fromNielsenSourceWatermarkStatusType ::
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

pattern NielsenSourceWatermarkStatusType_CLEAN :: NielsenSourceWatermarkStatusType
pattern NielsenSourceWatermarkStatusType_CLEAN = NielsenSourceWatermarkStatusType' "CLEAN"

pattern NielsenSourceWatermarkStatusType_WATERMARKED :: NielsenSourceWatermarkStatusType
pattern NielsenSourceWatermarkStatusType_WATERMARKED = NielsenSourceWatermarkStatusType' "WATERMARKED"

{-# COMPLETE
  NielsenSourceWatermarkStatusType_CLEAN,
  NielsenSourceWatermarkStatusType_WATERMARKED,
  NielsenSourceWatermarkStatusType'
  #-}
