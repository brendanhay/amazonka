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
-- Module      : Network.AWS.EC2.Types.AllocationStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AllocationStrategy
  ( AllocationStrategy
      ( ..,
        AllocationStrategy_CapacityOptimized,
        AllocationStrategy_Diversified,
        AllocationStrategy_LowestPrice
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype AllocationStrategy = AllocationStrategy'
  { fromAllocationStrategy ::
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

pattern AllocationStrategy_CapacityOptimized :: AllocationStrategy
pattern AllocationStrategy_CapacityOptimized = AllocationStrategy' "capacityOptimized"

pattern AllocationStrategy_Diversified :: AllocationStrategy
pattern AllocationStrategy_Diversified = AllocationStrategy' "diversified"

pattern AllocationStrategy_LowestPrice :: AllocationStrategy
pattern AllocationStrategy_LowestPrice = AllocationStrategy' "lowestPrice"

{-# COMPLETE
  AllocationStrategy_CapacityOptimized,
  AllocationStrategy_Diversified,
  AllocationStrategy_LowestPrice,
  AllocationStrategy'
  #-}
