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
-- Module      : Network.AWS.EC2.Types.SpotAllocationStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotAllocationStrategy
  ( SpotAllocationStrategy
      ( ..,
        SpotAllocationStrategy_Capacity_optimized,
        SpotAllocationStrategy_Diversified,
        SpotAllocationStrategy_Lowest_price
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype SpotAllocationStrategy = SpotAllocationStrategy'
  { fromSpotAllocationStrategy ::
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

pattern SpotAllocationStrategy_Capacity_optimized :: SpotAllocationStrategy
pattern SpotAllocationStrategy_Capacity_optimized = SpotAllocationStrategy' "capacity-optimized"

pattern SpotAllocationStrategy_Diversified :: SpotAllocationStrategy
pattern SpotAllocationStrategy_Diversified = SpotAllocationStrategy' "diversified"

pattern SpotAllocationStrategy_Lowest_price :: SpotAllocationStrategy
pattern SpotAllocationStrategy_Lowest_price = SpotAllocationStrategy' "lowest-price"

{-# COMPLETE
  SpotAllocationStrategy_Capacity_optimized,
  SpotAllocationStrategy_Diversified,
  SpotAllocationStrategy_Lowest_price,
  SpotAllocationStrategy'
  #-}
