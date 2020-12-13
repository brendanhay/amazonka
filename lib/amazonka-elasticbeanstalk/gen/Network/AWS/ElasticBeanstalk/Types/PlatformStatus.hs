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
        PSCreating,
        PSFailed,
        PSReady,
        PSDeleting,
        PSDeleted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PlatformStatus = PlatformStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PSCreating :: PlatformStatus
pattern PSCreating = PlatformStatus' "Creating"

pattern PSFailed :: PlatformStatus
pattern PSFailed = PlatformStatus' "Failed"

pattern PSReady :: PlatformStatus
pattern PSReady = PlatformStatus' "Ready"

pattern PSDeleting :: PlatformStatus
pattern PSDeleting = PlatformStatus' "Deleting"

pattern PSDeleted :: PlatformStatus
pattern PSDeleted = PlatformStatus' "Deleted"

{-# COMPLETE
  PSCreating,
  PSFailed,
  PSReady,
  PSDeleting,
  PSDeleted,
  PlatformStatus'
  #-}
