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
-- Module      : Amazonka.Snowball.Types.ShippingOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.ShippingOption
  ( ShippingOption
      ( ..,
        ShippingOption_EXPRESS,
        ShippingOption_NEXT_DAY,
        ShippingOption_SECOND_DAY,
        ShippingOption_STANDARD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ShippingOption = ShippingOption'
  { fromShippingOption ::
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

pattern ShippingOption_EXPRESS :: ShippingOption
pattern ShippingOption_EXPRESS = ShippingOption' "EXPRESS"

pattern ShippingOption_NEXT_DAY :: ShippingOption
pattern ShippingOption_NEXT_DAY = ShippingOption' "NEXT_DAY"

pattern ShippingOption_SECOND_DAY :: ShippingOption
pattern ShippingOption_SECOND_DAY = ShippingOption' "SECOND_DAY"

pattern ShippingOption_STANDARD :: ShippingOption
pattern ShippingOption_STANDARD = ShippingOption' "STANDARD"

{-# COMPLETE
  ShippingOption_EXPRESS,
  ShippingOption_NEXT_DAY,
  ShippingOption_SECOND_DAY,
  ShippingOption_STANDARD,
  ShippingOption'
  #-}
