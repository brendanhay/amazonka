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
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothAudioDeduplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothAudioDeduplication
  ( MsSmoothAudioDeduplication
      ( ..,
        MsSmoothAudioDeduplication_COMBINE_DUPLICATE_STREAMS,
        MsSmoothAudioDeduplication_NONE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings
-- across a Microsoft Smooth output group into a single audio stream.
newtype MsSmoothAudioDeduplication = MsSmoothAudioDeduplication'
  { fromMsSmoothAudioDeduplication ::
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

pattern MsSmoothAudioDeduplication_COMBINE_DUPLICATE_STREAMS :: MsSmoothAudioDeduplication
pattern MsSmoothAudioDeduplication_COMBINE_DUPLICATE_STREAMS = MsSmoothAudioDeduplication' "COMBINE_DUPLICATE_STREAMS"

pattern MsSmoothAudioDeduplication_NONE :: MsSmoothAudioDeduplication
pattern MsSmoothAudioDeduplication_NONE = MsSmoothAudioDeduplication' "NONE"

{-# COMPLETE
  MsSmoothAudioDeduplication_COMBINE_DUPLICATE_STREAMS,
  MsSmoothAudioDeduplication_NONE,
  MsSmoothAudioDeduplication'
  #-}
