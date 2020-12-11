-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.EntityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.EntityStatus
  ( EntityStatus
      ( EntityStatus',
        ESCompleted,
        ESDeleted,
        ESFailed,
        ESInprogress,
        ESPending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

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
newtype EntityStatus = EntityStatus' Lude.Text
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

pattern ESCompleted :: EntityStatus
pattern ESCompleted = EntityStatus' "COMPLETED"

pattern ESDeleted :: EntityStatus
pattern ESDeleted = EntityStatus' "DELETED"

pattern ESFailed :: EntityStatus
pattern ESFailed = EntityStatus' "FAILED"

pattern ESInprogress :: EntityStatus
pattern ESInprogress = EntityStatus' "INPROGRESS"

pattern ESPending :: EntityStatus
pattern ESPending = EntityStatus' "PENDING"

{-# COMPLETE
  ESCompleted,
  ESDeleted,
  ESFailed,
  ESInprogress,
  ESPending,
  EntityStatus'
  #-}
