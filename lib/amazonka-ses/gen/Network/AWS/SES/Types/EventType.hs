{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.EventType
  ( EventType
      ( EventType',
        ETBounce,
        ETClick,
        ETComplaint,
        ETDelivery,
        ETOpen,
        ETReject,
        ETRenderingFailure,
        ETSend
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EventType = EventType' Lude.Text
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

pattern ETBounce :: EventType
pattern ETBounce = EventType' "bounce"

pattern ETClick :: EventType
pattern ETClick = EventType' "click"

pattern ETComplaint :: EventType
pattern ETComplaint = EventType' "complaint"

pattern ETDelivery :: EventType
pattern ETDelivery = EventType' "delivery"

pattern ETOpen :: EventType
pattern ETOpen = EventType' "open"

pattern ETReject :: EventType
pattern ETReject = EventType' "reject"

pattern ETRenderingFailure :: EventType
pattern ETRenderingFailure = EventType' "renderingFailure"

pattern ETSend :: EventType
pattern ETSend = EventType' "send"

{-# COMPLETE
  ETBounce,
  ETClick,
  ETComplaint,
  ETDelivery,
  ETOpen,
  ETReject,
  ETRenderingFailure,
  ETSend,
  EventType'
  #-}
