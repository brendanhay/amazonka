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
-- Module      : Amazonka.MediaConvert.Types.HlsProgramDateTime
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsProgramDateTime
  ( HlsProgramDateTime
      ( ..,
        HlsProgramDateTime_EXCLUDE,
        HlsProgramDateTime_INCLUDE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated as follows: either the program date and
-- time are initialized using the input timecode source, or the time is
-- initialized using the input timecode source and the date is initialized
-- using the timestamp_offset.
newtype HlsProgramDateTime = HlsProgramDateTime'
  { fromHlsProgramDateTime ::
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

pattern HlsProgramDateTime_EXCLUDE :: HlsProgramDateTime
pattern HlsProgramDateTime_EXCLUDE = HlsProgramDateTime' "EXCLUDE"

pattern HlsProgramDateTime_INCLUDE :: HlsProgramDateTime
pattern HlsProgramDateTime_INCLUDE = HlsProgramDateTime' "INCLUDE"

{-# COMPLETE
  HlsProgramDateTime_EXCLUDE,
  HlsProgramDateTime_INCLUDE,
  HlsProgramDateTime'
  #-}
