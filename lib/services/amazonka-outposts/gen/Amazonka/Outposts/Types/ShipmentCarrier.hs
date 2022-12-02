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
-- Module      : Amazonka.Outposts.Types.ShipmentCarrier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.ShipmentCarrier
  ( ShipmentCarrier
      ( ..,
        ShipmentCarrier_DBS,
        ShipmentCarrier_DHL,
        ShipmentCarrier_FEDEX,
        ShipmentCarrier_UPS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ShipmentCarrier = ShipmentCarrier'
  { fromShipmentCarrier ::
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

pattern ShipmentCarrier_DBS :: ShipmentCarrier
pattern ShipmentCarrier_DBS = ShipmentCarrier' "DBS"

pattern ShipmentCarrier_DHL :: ShipmentCarrier
pattern ShipmentCarrier_DHL = ShipmentCarrier' "DHL"

pattern ShipmentCarrier_FEDEX :: ShipmentCarrier
pattern ShipmentCarrier_FEDEX = ShipmentCarrier' "FEDEX"

pattern ShipmentCarrier_UPS :: ShipmentCarrier
pattern ShipmentCarrier_UPS = ShipmentCarrier' "UPS"

{-# COMPLETE
  ShipmentCarrier_DBS,
  ShipmentCarrier_DHL,
  ShipmentCarrier_FEDEX,
  ShipmentCarrier_UPS,
  ShipmentCarrier'
  #-}
