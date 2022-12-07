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
-- Module      : Amazonka.EC2.Types.FleetOnDemandAllocationStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetOnDemandAllocationStrategy
  ( FleetOnDemandAllocationStrategy
      ( ..,
        FleetOnDemandAllocationStrategy_Lowest_price,
        FleetOnDemandAllocationStrategy_Prioritized
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype FleetOnDemandAllocationStrategy = FleetOnDemandAllocationStrategy'
  { fromFleetOnDemandAllocationStrategy ::
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

pattern FleetOnDemandAllocationStrategy_Lowest_price :: FleetOnDemandAllocationStrategy
pattern FleetOnDemandAllocationStrategy_Lowest_price = FleetOnDemandAllocationStrategy' "lowest-price"

pattern FleetOnDemandAllocationStrategy_Prioritized :: FleetOnDemandAllocationStrategy
pattern FleetOnDemandAllocationStrategy_Prioritized = FleetOnDemandAllocationStrategy' "prioritized"

{-# COMPLETE
  FleetOnDemandAllocationStrategy_Lowest_price,
  FleetOnDemandAllocationStrategy_Prioritized,
  FleetOnDemandAllocationStrategy'
  #-}
