{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformStatus
  ( PlatformStatus
      ( PlatformStatus',
        PlatformStatusCreating,
        PlatformStatusFailed,
        PlatformStatusReady,
        PlatformStatusDeleting,
        PlatformStatusDeleted,
        fromPlatformStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PlatformStatus = PlatformStatus'
  { fromPlatformStatus ::
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

pattern PlatformStatusCreating :: PlatformStatus
pattern PlatformStatusCreating = PlatformStatus' "Creating"

pattern PlatformStatusFailed :: PlatformStatus
pattern PlatformStatusFailed = PlatformStatus' "Failed"

pattern PlatformStatusReady :: PlatformStatus
pattern PlatformStatusReady = PlatformStatus' "Ready"

pattern PlatformStatusDeleting :: PlatformStatus
pattern PlatformStatusDeleting = PlatformStatus' "Deleting"

pattern PlatformStatusDeleted :: PlatformStatus
pattern PlatformStatusDeleted = PlatformStatus' "Deleted"

{-# COMPLETE
  PlatformStatusCreating,
  PlatformStatusFailed,
  PlatformStatusReady,
  PlatformStatusDeleting,
  PlatformStatusDeleted,
  PlatformStatus'
  #-}
