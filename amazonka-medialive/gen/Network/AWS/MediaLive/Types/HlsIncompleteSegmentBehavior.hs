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
-- Module      : Network.AWS.MediaLive.Types.HlsIncompleteSegmentBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsIncompleteSegmentBehavior
  ( HlsIncompleteSegmentBehavior
      ( ..,
        HlsIncompleteSegmentBehavior_AUTO,
        HlsIncompleteSegmentBehavior_SUPPRESS
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Hls Incomplete Segment Behavior
newtype HlsIncompleteSegmentBehavior = HlsIncompleteSegmentBehavior'
  { fromHlsIncompleteSegmentBehavior ::
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

pattern HlsIncompleteSegmentBehavior_AUTO :: HlsIncompleteSegmentBehavior
pattern HlsIncompleteSegmentBehavior_AUTO = HlsIncompleteSegmentBehavior' "AUTO"

pattern HlsIncompleteSegmentBehavior_SUPPRESS :: HlsIncompleteSegmentBehavior
pattern HlsIncompleteSegmentBehavior_SUPPRESS = HlsIncompleteSegmentBehavior' "SUPPRESS"

{-# COMPLETE
  HlsIncompleteSegmentBehavior_AUTO,
  HlsIncompleteSegmentBehavior_SUPPRESS,
  HlsIncompleteSegmentBehavior'
  #-}
