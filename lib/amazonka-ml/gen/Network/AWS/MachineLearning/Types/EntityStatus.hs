{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.EntityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.EntityStatus
  ( EntityStatus
    ( EntityStatus'
    , EntityStatusPending
    , EntityStatusInprogress
    , EntityStatusFailed
    , EntityStatusCompleted
    , EntityStatusDeleted
    , fromEntityStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Object status with the following possible values:
--
--
--     * @PENDING@ 
--
--     * @INPROGRESS@ 
--
--     * @FAILED@ 
--
--     * @COMPLETED@ 
--
--     * @DELETED@ 
--
newtype EntityStatus = EntityStatus'{fromEntityStatus :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern EntityStatusPending :: EntityStatus
pattern EntityStatusPending = EntityStatus' "PENDING"

pattern EntityStatusInprogress :: EntityStatus
pattern EntityStatusInprogress = EntityStatus' "INPROGRESS"

pattern EntityStatusFailed :: EntityStatus
pattern EntityStatusFailed = EntityStatus' "FAILED"

pattern EntityStatusCompleted :: EntityStatus
pattern EntityStatusCompleted = EntityStatus' "COMPLETED"

pattern EntityStatusDeleted :: EntityStatus
pattern EntityStatusDeleted = EntityStatus' "DELETED"

{-# COMPLETE 
  EntityStatusPending,

  EntityStatusInprogress,

  EntityStatusFailed,

  EntityStatusCompleted,

  EntityStatusDeleted,
  EntityStatus'
  #-}
