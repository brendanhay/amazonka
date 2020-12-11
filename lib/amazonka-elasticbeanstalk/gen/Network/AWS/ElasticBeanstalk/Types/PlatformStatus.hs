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
        Creating,
        Deleted,
        Deleting,
        Failed,
        Ready
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

pattern Creating :: PlatformStatus
pattern Creating = PlatformStatus' "Creating"

pattern Deleted :: PlatformStatus
pattern Deleted = PlatformStatus' "Deleted"

pattern Deleting :: PlatformStatus
pattern Deleting = PlatformStatus' "Deleting"

pattern Failed :: PlatformStatus
pattern Failed = PlatformStatus' "Failed"

pattern Ready :: PlatformStatus
pattern Ready = PlatformStatus' "Ready"

{-# COMPLETE
  Creating,
  Deleted,
  Deleting,
  Failed,
  Ready,
  PlatformStatus'
  #-}
