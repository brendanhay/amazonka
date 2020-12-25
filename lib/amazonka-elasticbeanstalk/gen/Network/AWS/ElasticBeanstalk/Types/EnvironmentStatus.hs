{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentStatus
  ( EnvironmentStatus
      ( EnvironmentStatus',
        EnvironmentStatusAborting,
        EnvironmentStatusLaunching,
        EnvironmentStatusUpdating,
        EnvironmentStatusLinkingFrom,
        EnvironmentStatusLinkingTo,
        EnvironmentStatusReady,
        EnvironmentStatusTerminating,
        EnvironmentStatusTerminated,
        fromEnvironmentStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EnvironmentStatus = EnvironmentStatus'
  { fromEnvironmentStatus ::
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

pattern EnvironmentStatusAborting :: EnvironmentStatus
pattern EnvironmentStatusAborting = EnvironmentStatus' "Aborting"

pattern EnvironmentStatusLaunching :: EnvironmentStatus
pattern EnvironmentStatusLaunching = EnvironmentStatus' "Launching"

pattern EnvironmentStatusUpdating :: EnvironmentStatus
pattern EnvironmentStatusUpdating = EnvironmentStatus' "Updating"

pattern EnvironmentStatusLinkingFrom :: EnvironmentStatus
pattern EnvironmentStatusLinkingFrom = EnvironmentStatus' "LinkingFrom"

pattern EnvironmentStatusLinkingTo :: EnvironmentStatus
pattern EnvironmentStatusLinkingTo = EnvironmentStatus' "LinkingTo"

pattern EnvironmentStatusReady :: EnvironmentStatus
pattern EnvironmentStatusReady = EnvironmentStatus' "Ready"

pattern EnvironmentStatusTerminating :: EnvironmentStatus
pattern EnvironmentStatusTerminating = EnvironmentStatus' "Terminating"

pattern EnvironmentStatusTerminated :: EnvironmentStatus
pattern EnvironmentStatusTerminated = EnvironmentStatus' "Terminated"

{-# COMPLETE
  EnvironmentStatusAborting,
  EnvironmentStatusLaunching,
  EnvironmentStatusUpdating,
  EnvironmentStatusLinkingFrom,
  EnvironmentStatusLinkingTo,
  EnvironmentStatusReady,
  EnvironmentStatusTerminating,
  EnvironmentStatusTerminated,
  EnvironmentStatus'
  #-}
