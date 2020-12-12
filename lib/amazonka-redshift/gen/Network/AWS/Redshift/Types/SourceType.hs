{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SourceType
  ( SourceType
      ( SourceType',
        Cluster,
        ClusterParameterGroup,
        ClusterSecurityGroup,
        ClusterSnapshot,
        ScheduledAction
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

newtype SourceType = SourceType' Lude.Text
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

pattern Cluster :: SourceType
pattern Cluster = SourceType' "cluster"

pattern ClusterParameterGroup :: SourceType
pattern ClusterParameterGroup = SourceType' "cluster-parameter-group"

pattern ClusterSecurityGroup :: SourceType
pattern ClusterSecurityGroup = SourceType' "cluster-security-group"

pattern ClusterSnapshot :: SourceType
pattern ClusterSnapshot = SourceType' "cluster-snapshot"

pattern ScheduledAction :: SourceType
pattern ScheduledAction = SourceType' "scheduled-action"

{-# COMPLETE
  Cluster,
  ClusterParameterGroup,
  ClusterSecurityGroup,
  ClusterSnapshot,
  ScheduledAction,
  SourceType'
  #-}
