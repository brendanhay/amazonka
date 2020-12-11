-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamStatus
  ( DeliveryStreamStatus
      ( DeliveryStreamStatus',
        Active,
        Creating,
        CreatingFailed,
        Deleting,
        DeletingFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeliveryStreamStatus = DeliveryStreamStatus' Lude.Text
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

pattern Active :: DeliveryStreamStatus
pattern Active = DeliveryStreamStatus' "ACTIVE"

pattern Creating :: DeliveryStreamStatus
pattern Creating = DeliveryStreamStatus' "CREATING"

pattern CreatingFailed :: DeliveryStreamStatus
pattern CreatingFailed = DeliveryStreamStatus' "CREATING_FAILED"

pattern Deleting :: DeliveryStreamStatus
pattern Deleting = DeliveryStreamStatus' "DELETING"

pattern DeletingFailed :: DeliveryStreamStatus
pattern DeletingFailed = DeliveryStreamStatus' "DELETING_FAILED"

{-# COMPLETE
  Active,
  Creating,
  CreatingFailed,
  Deleting,
  DeletingFailed,
  DeliveryStreamStatus'
  #-}
