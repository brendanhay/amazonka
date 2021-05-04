{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdCaptionContainerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdCaptionContainerType
  ( MpdCaptionContainerType
      ( ..,
        MpdCaptionContainerType_FRAGMENTED_MP4,
        MpdCaptionContainerType_RAW
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Use this setting only in DASH output groups that include sidecar TTML or
-- IMSC captions. You specify sidecar captions in a separate output from
-- your audio and video. Choose Raw (RAW) for captions in a single XML file
-- in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for
-- captions in XML format contained within fragmented MP4 files. This set
-- of fragmented MP4 files is separate from your video and audio fragmented
-- MP4 files.
newtype MpdCaptionContainerType = MpdCaptionContainerType'
  { fromMpdCaptionContainerType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
