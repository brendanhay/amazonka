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
-- Module      : Amazonka.MediaLive.Types.IFrameOnlyPlaylistType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.IFrameOnlyPlaylistType
  ( IFrameOnlyPlaylistType
      ( ..,
        IFrameOnlyPlaylistType_DISABLED,
        IFrameOnlyPlaylistType_STANDARD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When set to \"standard\", an I-Frame only playlist will be written out
-- for each video output in the output group. This I-Frame only playlist
-- will contain byte range offsets pointing to the I-frame(s) in each
-- segment.
newtype IFrameOnlyPlaylistType = IFrameOnlyPlaylistType'
  { fromIFrameOnlyPlaylistType ::
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

pattern IFrameOnlyPlaylistType_DISABLED :: IFrameOnlyPlaylistType
pattern IFrameOnlyPlaylistType_DISABLED = IFrameOnlyPlaylistType' "DISABLED"

pattern IFrameOnlyPlaylistType_STANDARD :: IFrameOnlyPlaylistType
pattern IFrameOnlyPlaylistType_STANDARD = IFrameOnlyPlaylistType' "STANDARD"

{-# COMPLETE
  IFrameOnlyPlaylistType_DISABLED,
  IFrameOnlyPlaylistType_STANDARD,
  IFrameOnlyPlaylistType'
  #-}
