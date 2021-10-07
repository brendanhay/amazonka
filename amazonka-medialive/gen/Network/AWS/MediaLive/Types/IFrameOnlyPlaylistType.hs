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
-- Module      : Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType
  ( IFrameOnlyPlaylistType
      ( ..,
        IFrameOnlyPlaylistType_DISABLED,
        IFrameOnlyPlaylistType_STANDARD
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | When set to \"standard\", an I-Frame only playlist will be written out
-- for each video output in the output group. This I-Frame only playlist
-- will contain byte range offsets pointing to the I-frame(s) in each
-- segment.
newtype IFrameOnlyPlaylistType = IFrameOnlyPlaylistType'
  { fromIFrameOnlyPlaylistType ::
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

pattern IFrameOnlyPlaylistType_DISABLED :: IFrameOnlyPlaylistType
pattern IFrameOnlyPlaylistType_DISABLED = IFrameOnlyPlaylistType' "DISABLED"

pattern IFrameOnlyPlaylistType_STANDARD :: IFrameOnlyPlaylistType
pattern IFrameOnlyPlaylistType_STANDARD = IFrameOnlyPlaylistType' "STANDARD"

{-# COMPLETE
  IFrameOnlyPlaylistType_DISABLED,
  IFrameOnlyPlaylistType_STANDARD,
  IFrameOnlyPlaylistType'
  #-}
