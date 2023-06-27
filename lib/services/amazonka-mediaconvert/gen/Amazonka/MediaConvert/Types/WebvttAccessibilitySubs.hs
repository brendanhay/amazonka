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
-- Module      : Amazonka.MediaConvert.Types.WebvttAccessibilitySubs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.WebvttAccessibilitySubs
  ( WebvttAccessibilitySubs
      ( ..,
        WebvttAccessibilitySubs_DISABLED,
        WebvttAccessibilitySubs_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If the WebVTT captions track is intended to provide accessibility for
-- people who are deaf or hard of hearing: Set Accessibility subtitles to
-- Enabled. When you do, MediaConvert adds accessibility attributes to your
-- output HLS or DASH manifest. For HLS manifests, MediaConvert adds the
-- following accessibility attributes under EXT-X-MEDIA for this track:
-- CHARACTERISTICS=\"public.accessibility.describes-spoken-dialog,public.accessibility.describes-music-and-sound\"
-- and AUTOSELECT=\"YES\". For DASH manifests, MediaConvert adds the
-- following in the adaptation set for this track: . If the captions track
-- is not intended to provide such accessibility: Keep the default value,
-- Disabled. When you do, for DASH manifests, MediaConvert instead adds the
-- following in the adaptation set for this track: .
newtype WebvttAccessibilitySubs = WebvttAccessibilitySubs'
  { fromWebvttAccessibilitySubs ::
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

pattern WebvttAccessibilitySubs_DISABLED :: WebvttAccessibilitySubs
pattern WebvttAccessibilitySubs_DISABLED = WebvttAccessibilitySubs' "DISABLED"

pattern WebvttAccessibilitySubs_ENABLED :: WebvttAccessibilitySubs
pattern WebvttAccessibilitySubs_ENABLED = WebvttAccessibilitySubs' "ENABLED"

{-# COMPLETE
  WebvttAccessibilitySubs_DISABLED,
  WebvttAccessibilitySubs_ENABLED,
  WebvttAccessibilitySubs'
  #-}
