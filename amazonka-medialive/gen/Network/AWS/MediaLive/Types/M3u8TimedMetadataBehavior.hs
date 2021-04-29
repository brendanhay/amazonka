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
-- Module      : Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
  ( M3u8TimedMetadataBehavior
      ( ..,
        M3u8TimedMetadataBehavior_NO_PASSTHROUGH,
        M3u8TimedMetadataBehavior_PASSTHROUGH
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | M3u8 Timed Metadata Behavior
newtype M3u8TimedMetadataBehavior = M3u8TimedMetadataBehavior'
  { fromM3u8TimedMetadataBehavior ::
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

pattern M3u8TimedMetadataBehavior_NO_PASSTHROUGH :: M3u8TimedMetadataBehavior
pattern M3u8TimedMetadataBehavior_NO_PASSTHROUGH = M3u8TimedMetadataBehavior' "NO_PASSTHROUGH"

pattern M3u8TimedMetadataBehavior_PASSTHROUGH :: M3u8TimedMetadataBehavior
pattern M3u8TimedMetadataBehavior_PASSTHROUGH = M3u8TimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  M3u8TimedMetadataBehavior_NO_PASSTHROUGH,
  M3u8TimedMetadataBehavior_PASSTHROUGH,
  M3u8TimedMetadataBehavior'
  #-}
