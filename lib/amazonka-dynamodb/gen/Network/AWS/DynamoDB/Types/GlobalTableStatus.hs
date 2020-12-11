-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTableStatus
  ( GlobalTableStatus
      ( GlobalTableStatus',
        GTSActive,
        GTSCreating,
        GTSDeleting,
        GTSUpdating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GlobalTableStatus = GlobalTableStatus' Lude.Text
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

pattern GTSActive :: GlobalTableStatus
pattern GTSActive = GlobalTableStatus' "ACTIVE"

pattern GTSCreating :: GlobalTableStatus
pattern GTSCreating = GlobalTableStatus' "CREATING"

pattern GTSDeleting :: GlobalTableStatus
pattern GTSDeleting = GlobalTableStatus' "DELETING"

pattern GTSUpdating :: GlobalTableStatus
pattern GTSUpdating = GlobalTableStatus' "UPDATING"

{-# COMPLETE
  GTSActive,
  GTSCreating,
  GTSDeleting,
  GTSUpdating,
  GlobalTableStatus'
  #-}
