{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ShippingLabelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ShippingLabelStatus
  ( ShippingLabelStatus
      ( ShippingLabelStatus',
        Failed,
        InProgress,
        Succeeded,
        TimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ShippingLabelStatus = ShippingLabelStatus' Lude.Text
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

pattern Failed :: ShippingLabelStatus
pattern Failed = ShippingLabelStatus' "Failed"

pattern InProgress :: ShippingLabelStatus
pattern InProgress = ShippingLabelStatus' "InProgress"

pattern Succeeded :: ShippingLabelStatus
pattern Succeeded = ShippingLabelStatus' "Succeeded"

pattern TimedOut :: ShippingLabelStatus
pattern TimedOut = ShippingLabelStatus' "TimedOut"

{-# COMPLETE
  Failed,
  InProgress,
  Succeeded,
  TimedOut,
  ShippingLabelStatus'
  #-}
