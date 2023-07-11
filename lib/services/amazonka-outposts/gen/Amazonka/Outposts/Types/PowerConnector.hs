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
-- Module      : Amazonka.Outposts.Types.PowerConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.PowerConnector
  ( PowerConnector
      ( ..,
        PowerConnector_AH530P7W,
        PowerConnector_AH532P6W,
        PowerConnector_IEC309,
        PowerConnector_L6_30P
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PowerConnector = PowerConnector'
  { fromPowerConnector ::
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

pattern PowerConnector_AH530P7W :: PowerConnector
pattern PowerConnector_AH530P7W = PowerConnector' "AH530P7W"

pattern PowerConnector_AH532P6W :: PowerConnector
pattern PowerConnector_AH532P6W = PowerConnector' "AH532P6W"

pattern PowerConnector_IEC309 :: PowerConnector
pattern PowerConnector_IEC309 = PowerConnector' "IEC309"

pattern PowerConnector_L6_30P :: PowerConnector
pattern PowerConnector_L6_30P = PowerConnector' "L6_30P"

{-# COMPLETE
  PowerConnector_AH530P7W,
  PowerConnector_AH532P6W,
  PowerConnector_IEC309,
  PowerConnector_L6_30P,
  PowerConnector'
  #-}
