{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Fmp4TimedMetadataBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Fmp4TimedMetadataBehavior
  ( Fmp4TimedMetadataBehavior
      ( Fmp4TimedMetadataBehavior',
        Fmp4TimedMetadataBehaviorNoPassthrough,
        Fmp4TimedMetadataBehaviorPassthrough,
        fromFmp4TimedMetadataBehavior
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Fmp4 Timed Metadata Behavior
newtype Fmp4TimedMetadataBehavior = Fmp4TimedMetadataBehavior'
  { fromFmp4TimedMetadataBehavior ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern Fmp4TimedMetadataBehaviorNoPassthrough :: Fmp4TimedMetadataBehavior
pattern Fmp4TimedMetadataBehaviorNoPassthrough = Fmp4TimedMetadataBehavior' "NO_PASSTHROUGH"

pattern Fmp4TimedMetadataBehaviorPassthrough :: Fmp4TimedMetadataBehavior
pattern Fmp4TimedMetadataBehaviorPassthrough = Fmp4TimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  Fmp4TimedMetadataBehaviorNoPassthrough,
  Fmp4TimedMetadataBehaviorPassthrough,
  Fmp4TimedMetadataBehavior'
  #-}
