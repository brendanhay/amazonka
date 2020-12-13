{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Pending,
        Inprogress,
        Failed,
        Completed,
        Deleted
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

pattern Pending :: EntityStatus
pattern Pending = EntityStatus' "PENDING"

pattern Inprogress :: EntityStatus
pattern Inprogress = EntityStatus' "INPROGRESS"

pattern Failed :: EntityStatus
pattern Failed = EntityStatus' "FAILED"

pattern Completed :: EntityStatus
pattern Completed = EntityStatus' "COMPLETED"

pattern Deleted :: EntityStatus
pattern Deleted = EntityStatus' "DELETED"

{-# COMPLETE
  Pending,
  Inprogress,
  Failed,
  Completed,
  Deleted,
  EntityStatus'
  #-}
