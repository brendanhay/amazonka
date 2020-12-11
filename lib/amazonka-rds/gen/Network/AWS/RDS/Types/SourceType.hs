-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.SourceType
  ( SourceType
      ( SourceType',
        DBCluster,
        DBClusterSnapshot,
        DBInstance,
        DBParameterGroup,
        DBSecurityGroup,
        DBSnapshot
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

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

pattern DBCluster :: SourceType
pattern DBCluster = SourceType' "db-cluster"

pattern DBClusterSnapshot :: SourceType
pattern DBClusterSnapshot = SourceType' "db-cluster-snapshot"

pattern DBInstance :: SourceType
pattern DBInstance = SourceType' "db-instance"

pattern DBParameterGroup :: SourceType
pattern DBParameterGroup = SourceType' "db-parameter-group"

pattern DBSecurityGroup :: SourceType
pattern DBSecurityGroup = SourceType' "db-security-group"

pattern DBSnapshot :: SourceType
pattern DBSnapshot = SourceType' "db-snapshot"

{-# COMPLETE
  DBCluster,
  DBClusterSnapshot,
  DBInstance,
  DBParameterGroup,
  DBSecurityGroup,
  DBSnapshot,
  SourceType'
  #-}
