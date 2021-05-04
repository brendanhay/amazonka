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
-- Module      : Network.AWS.MediaConvert.Types.HlsSegmentControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsSegmentControl
  ( HlsSegmentControl
      ( ..,
        HlsSegmentControl_SEGMENTED_FILES,
        HlsSegmentControl_SINGLE_FILE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts)
-- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
newtype HlsSegmentControl = HlsSegmentControl'
  { fromHlsSegmentControl ::
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

pattern HlsSegmentControl_SEGMENTED_FILES :: HlsSegmentControl
pattern HlsSegmentControl_SEGMENTED_FILES = HlsSegmentControl' "SEGMENTED_FILES"

pattern HlsSegmentControl_SINGLE_FILE :: HlsSegmentControl
pattern HlsSegmentControl_SINGLE_FILE = HlsSegmentControl' "SINGLE_FILE"

{-# COMPLETE
  HlsSegmentControl_SEGMENTED_FILES,
  HlsSegmentControl_SINGLE_FILE,
  HlsSegmentControl'
  #-}
