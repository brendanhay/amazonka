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
        Aborting,
        Launching,
        Updating,
        LinkingFrom,
        LinkingTo,
        Ready,
        Terminating,
        Terminated
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

pattern Aborting :: EnvironmentStatus
pattern Aborting = EnvironmentStatus' "Aborting"

pattern Launching :: EnvironmentStatus
pattern Launching = EnvironmentStatus' "Launching"

pattern Updating :: EnvironmentStatus
pattern Updating = EnvironmentStatus' "Updating"

pattern LinkingFrom :: EnvironmentStatus
pattern LinkingFrom = EnvironmentStatus' "LinkingFrom"

pattern LinkingTo :: EnvironmentStatus
pattern LinkingTo = EnvironmentStatus' "LinkingTo"

pattern Ready :: EnvironmentStatus
pattern Ready = EnvironmentStatus' "Ready"

pattern Terminating :: EnvironmentStatus
pattern Terminating = EnvironmentStatus' "Terminating"

pattern Terminated :: EnvironmentStatus
pattern Terminated = EnvironmentStatus' "Terminated"

{-# COMPLETE
  Aborting,
  Launching,
  Updating,
  LinkingFrom,
  LinkingTo,
  Ready,
  Terminating,
  Terminated,
  EnvironmentStatus'
  #-}
