{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.SourceType
  ( SourceType
    ( SourceType'
    , SourceTypeDbInstance
    , SourceTypeDbParameterGroup
    , SourceTypeDbSecurityGroup
    , SourceTypeDbSnapshot
    , SourceTypeDbCluster
    , SourceTypeDbClusterSnapshot
    , fromSourceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SourceType = SourceType'{fromSourceType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern SourceTypeDbInstance :: SourceType
pattern SourceTypeDbInstance = SourceType' "db-instance"

pattern SourceTypeDbParameterGroup :: SourceType
pattern SourceTypeDbParameterGroup = SourceType' "db-parameter-group"

pattern SourceTypeDbSecurityGroup :: SourceType
pattern SourceTypeDbSecurityGroup = SourceType' "db-security-group"

pattern SourceTypeDbSnapshot :: SourceType
pattern SourceTypeDbSnapshot = SourceType' "db-snapshot"

pattern SourceTypeDbCluster :: SourceType
pattern SourceTypeDbCluster = SourceType' "db-cluster"

pattern SourceTypeDbClusterSnapshot :: SourceType
pattern SourceTypeDbClusterSnapshot = SourceType' "db-cluster-snapshot"

{-# COMPLETE 
  SourceTypeDbInstance,

  SourceTypeDbParameterGroup,

  SourceTypeDbSecurityGroup,

  SourceTypeDbSnapshot,

  SourceTypeDbCluster,

  SourceTypeDbClusterSnapshot,
  SourceType'
  #-}
