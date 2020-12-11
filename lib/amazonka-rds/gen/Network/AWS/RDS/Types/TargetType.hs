-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetType
  ( TargetType
      ( TargetType',
        RDSInstance,
        RDSServerlessEndpoint,
        TrackedCluster
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TargetType = TargetType' Lude.Text
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

pattern RDSInstance :: TargetType
pattern RDSInstance = TargetType' "RDS_INSTANCE"

pattern RDSServerlessEndpoint :: TargetType
pattern RDSServerlessEndpoint = TargetType' "RDS_SERVERLESS_ENDPOINT"

pattern TrackedCluster :: TargetType
pattern TrackedCluster = TargetType' "TRACKED_CLUSTER"

{-# COMPLETE
  RDSInstance,
  RDSServerlessEndpoint,
  TrackedCluster,
  TargetType'
  #-}
