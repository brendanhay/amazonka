{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior
  ( M2tsTimedMetadataBehavior
      ( M2tsTimedMetadataBehavior',
        M2tsTimedMetadataBehaviorNoPassthrough,
        M2tsTimedMetadataBehaviorPassthrough,
        fromM2tsTimedMetadataBehavior
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | M2ts Timed Metadata Behavior
newtype M2tsTimedMetadataBehavior = M2tsTimedMetadataBehavior'
  { fromM2tsTimedMetadataBehavior ::
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

pattern M2tsTimedMetadataBehaviorNoPassthrough :: M2tsTimedMetadataBehavior
pattern M2tsTimedMetadataBehaviorNoPassthrough = M2tsTimedMetadataBehavior' "NO_PASSTHROUGH"

pattern M2tsTimedMetadataBehaviorPassthrough :: M2tsTimedMetadataBehavior
pattern M2tsTimedMetadataBehaviorPassthrough = M2tsTimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  M2tsTimedMetadataBehaviorNoPassthrough,
  M2tsTimedMetadataBehaviorPassthrough,
  M2tsTimedMetadataBehavior'
  #-}
