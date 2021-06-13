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
-- Module      : Network.AWS.MediaPackage.Types.PlaylistType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.PlaylistType
  ( PlaylistType
      ( ..,
        PlaylistType_EVENT,
        PlaylistType_NONE,
        PlaylistType_VOD
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PlaylistType = PlaylistType'
  { fromPlaylistType ::
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

pattern PlaylistType_EVENT :: PlaylistType
pattern PlaylistType_EVENT = PlaylistType' "EVENT"

pattern PlaylistType_NONE :: PlaylistType
pattern PlaylistType_NONE = PlaylistType' "NONE"

pattern PlaylistType_VOD :: PlaylistType
pattern PlaylistType_VOD = PlaylistType' "VOD"

{-# COMPLETE
  PlaylistType_EVENT,
  PlaylistType_NONE,
  PlaylistType_VOD,
  PlaylistType'
  #-}
