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
-- Module      : Amazonka.MediaConvert.Types.NielsenSourceWatermarkStatusType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.NielsenSourceWatermarkStatusType
  ( NielsenSourceWatermarkStatusType
      ( ..,
        NielsenSourceWatermarkStatusType_CLEAN,
        NielsenSourceWatermarkStatusType_WATERMARKED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Required. Specify whether your source content already contains Nielsen
-- non-linear watermarks. When you set this value to Watermarked
-- (WATERMARKED), the service fails the job. Nielsen requires that you add
-- non-linear watermarking to only clean content that doesn\'t already have
-- non-linear Nielsen watermarks.
newtype NielsenSourceWatermarkStatusType = NielsenSourceWatermarkStatusType'
  { fromNielsenSourceWatermarkStatusType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
