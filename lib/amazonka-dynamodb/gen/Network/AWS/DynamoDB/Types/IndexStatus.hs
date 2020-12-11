-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.IndexStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.IndexStatus
  ( IndexStatus
      ( IndexStatus',
        ISActive,
        ISCreating,
        ISDeleting,
        ISUpdating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype IndexStatus = IndexStatus' Lude.Text
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

pattern ISActive :: IndexStatus
pattern ISActive = IndexStatus' "ACTIVE"

pattern ISCreating :: IndexStatus
pattern ISCreating = IndexStatus' "CREATING"

pattern ISDeleting :: IndexStatus
pattern ISDeleting = IndexStatus' "DELETING"

pattern ISUpdating :: IndexStatus
pattern ISUpdating = IndexStatus' "UPDATING"

{-# COMPLETE
  ISActive,
  ISCreating,
  ISDeleting,
  ISUpdating,
  IndexStatus'
  #-}
