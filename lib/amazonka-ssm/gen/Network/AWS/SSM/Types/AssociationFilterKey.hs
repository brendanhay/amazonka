{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationFilterKey
  ( AssociationFilterKey
    ( AssociationFilterKey'
    , AssociationFilterKeyInstanceId
    , AssociationFilterKeyName
    , AssociationFilterKeyAssociationId
    , AssociationFilterKeyAssociationStatusName
    , AssociationFilterKeyLastExecutedBefore
    , AssociationFilterKeyLastExecutedAfter
    , AssociationFilterKeyAssociationName
    , AssociationFilterKeyResourceGroupName
    , fromAssociationFilterKey
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AssociationFilterKey = AssociationFilterKey'{fromAssociationFilterKey
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern AssociationFilterKeyInstanceId :: AssociationFilterKey
pattern AssociationFilterKeyInstanceId = AssociationFilterKey' "InstanceId"

pattern AssociationFilterKeyName :: AssociationFilterKey
pattern AssociationFilterKeyName = AssociationFilterKey' "Name"

pattern AssociationFilterKeyAssociationId :: AssociationFilterKey
pattern AssociationFilterKeyAssociationId = AssociationFilterKey' "AssociationId"

pattern AssociationFilterKeyAssociationStatusName :: AssociationFilterKey
pattern AssociationFilterKeyAssociationStatusName = AssociationFilterKey' "AssociationStatusName"

pattern AssociationFilterKeyLastExecutedBefore :: AssociationFilterKey
pattern AssociationFilterKeyLastExecutedBefore = AssociationFilterKey' "LastExecutedBefore"

pattern AssociationFilterKeyLastExecutedAfter :: AssociationFilterKey
pattern AssociationFilterKeyLastExecutedAfter = AssociationFilterKey' "LastExecutedAfter"

pattern AssociationFilterKeyAssociationName :: AssociationFilterKey
pattern AssociationFilterKeyAssociationName = AssociationFilterKey' "AssociationName"

pattern AssociationFilterKeyResourceGroupName :: AssociationFilterKey
pattern AssociationFilterKeyResourceGroupName = AssociationFilterKey' "ResourceGroupName"

{-# COMPLETE 
  AssociationFilterKeyInstanceId,

  AssociationFilterKeyName,

  AssociationFilterKeyAssociationId,

  AssociationFilterKeyAssociationStatusName,

  AssociationFilterKeyLastExecutedBefore,

  AssociationFilterKeyLastExecutedAfter,

  AssociationFilterKeyAssociationName,

  AssociationFilterKeyResourceGroupName,
  AssociationFilterKey'
  #-}
