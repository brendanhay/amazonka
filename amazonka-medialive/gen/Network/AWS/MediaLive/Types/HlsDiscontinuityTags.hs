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
-- Module      : Network.AWS.MediaLive.Types.HlsDiscontinuityTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsDiscontinuityTags
  ( HlsDiscontinuityTags
      ( ..,
        HlsDiscontinuityTags_INSERT,
        HlsDiscontinuityTags_NEVER_INSERT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Hls Discontinuity Tags
newtype HlsDiscontinuityTags = HlsDiscontinuityTags'
  { fromHlsDiscontinuityTags ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern HlsDiscontinuityTags_INSERT :: HlsDiscontinuityTags
pattern HlsDiscontinuityTags_INSERT = HlsDiscontinuityTags' "INSERT"

pattern HlsDiscontinuityTags_NEVER_INSERT :: HlsDiscontinuityTags
pattern HlsDiscontinuityTags_NEVER_INSERT = HlsDiscontinuityTags' "NEVER_INSERT"

{-# COMPLETE
  HlsDiscontinuityTags_INSERT,
  HlsDiscontinuityTags_NEVER_INSERT,
  HlsDiscontinuityTags'
  #-}
