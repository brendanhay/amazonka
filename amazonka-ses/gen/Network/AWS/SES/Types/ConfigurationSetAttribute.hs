{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ConfigurationSetAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ConfigurationSetAttribute
  ( ConfigurationSetAttribute
      ( ..,
        ConfigurationSetAttribute_DeliveryOptions,
        ConfigurationSetAttribute_EventDestinations,
        ConfigurationSetAttribute_ReputationOptions,
        ConfigurationSetAttribute_TrackingOptions
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ConfigurationSetAttribute = ConfigurationSetAttribute'
  { fromConfigurationSetAttribute ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ConfigurationSetAttribute_DeliveryOptions :: ConfigurationSetAttribute
pattern ConfigurationSetAttribute_DeliveryOptions = ConfigurationSetAttribute' "deliveryOptions"

pattern ConfigurationSetAttribute_EventDestinations :: ConfigurationSetAttribute
pattern ConfigurationSetAttribute_EventDestinations = ConfigurationSetAttribute' "eventDestinations"

pattern ConfigurationSetAttribute_ReputationOptions :: ConfigurationSetAttribute
pattern ConfigurationSetAttribute_ReputationOptions = ConfigurationSetAttribute' "reputationOptions"

pattern ConfigurationSetAttribute_TrackingOptions :: ConfigurationSetAttribute
pattern ConfigurationSetAttribute_TrackingOptions = ConfigurationSetAttribute' "trackingOptions"

{-# COMPLETE
  ConfigurationSetAttribute_DeliveryOptions,
  ConfigurationSetAttribute_EventDestinations,
  ConfigurationSetAttribute_ReputationOptions,
  ConfigurationSetAttribute_TrackingOptions,
  ConfigurationSetAttribute'
  #-}
