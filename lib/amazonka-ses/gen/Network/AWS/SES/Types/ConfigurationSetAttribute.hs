-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ConfigurationSetAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ConfigurationSetAttribute
  ( ConfigurationSetAttribute
      ( ConfigurationSetAttribute',
        DeliveryOptions,
        EventDestinations,
        ReputationOptions,
        TrackingOptions
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConfigurationSetAttribute = ConfigurationSetAttribute' Lude.Text
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

pattern DeliveryOptions :: ConfigurationSetAttribute
pattern DeliveryOptions = ConfigurationSetAttribute' "deliveryOptions"

pattern EventDestinations :: ConfigurationSetAttribute
pattern EventDestinations = ConfigurationSetAttribute' "eventDestinations"

pattern ReputationOptions :: ConfigurationSetAttribute
pattern ReputationOptions = ConfigurationSetAttribute' "reputationOptions"

pattern TrackingOptions :: ConfigurationSetAttribute
pattern TrackingOptions = ConfigurationSetAttribute' "trackingOptions"

{-# COMPLETE
  DeliveryOptions,
  EventDestinations,
  ReputationOptions,
  TrackingOptions,
  ConfigurationSetAttribute'
  #-}
