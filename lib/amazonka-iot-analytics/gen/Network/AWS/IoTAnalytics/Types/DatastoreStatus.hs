{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreStatus
  ( DatastoreStatus
      ( DatastoreStatus',
        DSCreating,
        DSActive,
        DSDeleting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DatastoreStatus = DatastoreStatus' Lude.Text
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

pattern DSCreating :: DatastoreStatus
pattern DSCreating = DatastoreStatus' "CREATING"

pattern DSActive :: DatastoreStatus
pattern DSActive = DatastoreStatus' "ACTIVE"

pattern DSDeleting :: DatastoreStatus
pattern DSDeleting = DatastoreStatus' "DELETING"

{-# COMPLETE
  DSCreating,
  DSActive,
  DSDeleting,
  DatastoreStatus'
  #-}
