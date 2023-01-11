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
-- Module      : Amazonka.MediaConvert.Types.MpdCaptionContainerType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MpdCaptionContainerType
  ( MpdCaptionContainerType
      ( ..,
        MpdCaptionContainerType_FRAGMENTED_MP4,
        MpdCaptionContainerType_RAW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this setting only in DASH output groups that include sidecar TTML or
-- IMSC captions. You specify sidecar captions in a separate output from
-- your audio and video. Choose Raw (RAW) for captions in a single XML file
-- in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for
-- captions in XML format contained within fragmented MP4 files. This set
-- of fragmented MP4 files is separate from your video and audio fragmented
-- MP4 files.
newtype MpdCaptionContainerType = MpdCaptionContainerType'
  { fromMpdCaptionContainerType ::
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

pattern MpdCaptionContainerType_FRAGMENTED_MP4 :: MpdCaptionContainerType
pattern MpdCaptionContainerType_FRAGMENTED_MP4 = MpdCaptionContainerType' "FRAGMENTED_MP4"

pattern MpdCaptionContainerType_RAW :: MpdCaptionContainerType
pattern MpdCaptionContainerType_RAW = MpdCaptionContainerType' "RAW"

{-# COMPLETE
  MpdCaptionContainerType_FRAGMENTED_MP4,
  MpdCaptionContainerType_RAW,
  MpdCaptionContainerType'
  #-}
