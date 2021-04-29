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
-- Module      : Network.AWS.MediaConvert.Types.HlsProgramDateTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsProgramDateTime
  ( HlsProgramDateTime
      ( ..,
        HlsProgramDateTime_EXCLUDE,
        HlsProgramDateTime_INCLUDE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated as follows: either the program date and
-- time are initialized using the input timecode source, or the time is
-- initialized using the input timecode source and the date is initialized
-- using the timestamp_offset.
newtype HlsProgramDateTime = HlsProgramDateTime'
  { fromHlsProgramDateTime ::
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

pattern HlsProgramDateTime_EXCLUDE :: HlsProgramDateTime
pattern HlsProgramDateTime_EXCLUDE = HlsProgramDateTime' "EXCLUDE"

pattern HlsProgramDateTime_INCLUDE :: HlsProgramDateTime
pattern HlsProgramDateTime_INCLUDE = HlsProgramDateTime' "INCLUDE"

{-# COMPLETE
  HlsProgramDateTime_EXCLUDE,
  HlsProgramDateTime_INCLUDE,
  HlsProgramDateTime'
  #-}
