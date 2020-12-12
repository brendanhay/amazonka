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
        ESAborting,
        ESLaunching,
        ESLinkingFrom,
        ESLinkingTo,
        ESReady,
        ESTerminated,
        ESTerminating,
        ESUpdating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EnvironmentStatus = EnvironmentStatus' Lude.Text
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

pattern ESAborting :: EnvironmentStatus
pattern ESAborting = EnvironmentStatus' "Aborting"

pattern ESLaunching :: EnvironmentStatus
pattern ESLaunching = EnvironmentStatus' "Launching"

pattern ESLinkingFrom :: EnvironmentStatus
pattern ESLinkingFrom = EnvironmentStatus' "LinkingFrom"

pattern ESLinkingTo :: EnvironmentStatus
pattern ESLinkingTo = EnvironmentStatus' "LinkingTo"

pattern ESReady :: EnvironmentStatus
pattern ESReady = EnvironmentStatus' "Ready"

pattern ESTerminated :: EnvironmentStatus
pattern ESTerminated = EnvironmentStatus' "Terminated"

pattern ESTerminating :: EnvironmentStatus
pattern ESTerminating = EnvironmentStatus' "Terminating"

pattern ESUpdating :: EnvironmentStatus
pattern ESUpdating = EnvironmentStatus' "Updating"

{-# COMPLETE
  ESAborting,
  ESLaunching,
  ESLinkingFrom,
  ESLinkingTo,
  ESReady,
  ESTerminated,
  ESTerminating,
  ESUpdating,
  EnvironmentStatus'
  #-}
