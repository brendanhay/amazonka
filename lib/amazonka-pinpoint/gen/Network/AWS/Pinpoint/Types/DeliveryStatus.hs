-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DeliveryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DeliveryStatus
  ( DeliveryStatus
      ( DeliveryStatus',
        Duplicate,
        OptOut,
        PermanentFailure,
        Successful,
        TemporaryFailure,
        Throttled,
        UnknownFailure
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeliveryStatus = DeliveryStatus' Lude.Text
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

pattern Duplicate :: DeliveryStatus
pattern Duplicate = DeliveryStatus' "DUPLICATE"

pattern OptOut :: DeliveryStatus
pattern OptOut = DeliveryStatus' "OPT_OUT"

pattern PermanentFailure :: DeliveryStatus
pattern PermanentFailure = DeliveryStatus' "PERMANENT_FAILURE"

pattern Successful :: DeliveryStatus
pattern Successful = DeliveryStatus' "SUCCESSFUL"

pattern TemporaryFailure :: DeliveryStatus
pattern TemporaryFailure = DeliveryStatus' "TEMPORARY_FAILURE"

pattern Throttled :: DeliveryStatus
pattern Throttled = DeliveryStatus' "THROTTLED"

pattern UnknownFailure :: DeliveryStatus
pattern UnknownFailure = DeliveryStatus' "UNKNOWN_FAILURE"

{-# COMPLETE
  Duplicate,
  OptOut,
  PermanentFailure,
  Successful,
  TemporaryFailure,
  Throttled,
  UnknownFailure,
  DeliveryStatus'
  #-}
