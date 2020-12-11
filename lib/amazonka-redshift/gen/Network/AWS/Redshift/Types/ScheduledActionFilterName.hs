-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionFilterName
  ( ScheduledActionFilterName
      ( ScheduledActionFilterName',
        ClusterIdentifier,
        IAMRole
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

newtype ScheduledActionFilterName = ScheduledActionFilterName' Lude.Text
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

pattern ClusterIdentifier :: ScheduledActionFilterName
pattern ClusterIdentifier = ScheduledActionFilterName' "cluster-identifier"

pattern IAMRole :: ScheduledActionFilterName
pattern IAMRole = ScheduledActionFilterName' "iam-role"

{-# COMPLETE
  ClusterIdentifier,
  IAMRole,
  ScheduledActionFilterName'
  #-}
