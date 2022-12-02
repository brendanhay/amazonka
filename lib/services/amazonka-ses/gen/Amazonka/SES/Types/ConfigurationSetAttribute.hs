{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SES.Types.ConfigurationSetAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.ConfigurationSetAttribute
  ( ConfigurationSetAttribute
      ( ..,
        ConfigurationSetAttribute_DeliveryOptions,
        ConfigurationSetAttribute_EventDestinations,
        ConfigurationSetAttribute_ReputationOptions,
        ConfigurationSetAttribute_TrackingOptions
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfigurationSetAttribute = ConfigurationSetAttribute'
  { fromConfigurationSetAttribute ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
