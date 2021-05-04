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
-- Module      : Network.AWS.GameLift.Types.BalancingStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.BalancingStrategy
  ( BalancingStrategy
      ( ..,
        BalancingStrategy_ON_DEMAND_ONLY,
        BalancingStrategy_SPOT_ONLY,
        BalancingStrategy_SPOT_PREFERRED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype BalancingStrategy = BalancingStrategy'
  { fromBalancingStrategy ::
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

pattern BalancingStrategy_ON_DEMAND_ONLY :: BalancingStrategy
pattern BalancingStrategy_ON_DEMAND_ONLY = BalancingStrategy' "ON_DEMAND_ONLY"

pattern BalancingStrategy_SPOT_ONLY :: BalancingStrategy
pattern BalancingStrategy_SPOT_ONLY = BalancingStrategy' "SPOT_ONLY"

pattern BalancingStrategy_SPOT_PREFERRED :: BalancingStrategy
pattern BalancingStrategy_SPOT_PREFERRED = BalancingStrategy' "SPOT_PREFERRED"

{-# COMPLETE
  BalancingStrategy_ON_DEMAND_ONLY,
  BalancingStrategy_SPOT_ONLY,
  BalancingStrategy_SPOT_PREFERRED,
  BalancingStrategy'
  #-}
