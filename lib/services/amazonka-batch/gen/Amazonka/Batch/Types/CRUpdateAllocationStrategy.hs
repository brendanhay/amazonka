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
-- Module      : Amazonka.Batch.Types.CRUpdateAllocationStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.CRUpdateAllocationStrategy
  ( CRUpdateAllocationStrategy
      ( ..,
        CRUpdateAllocationStrategy_BEST_FIT_PROGRESSIVE,
        CRUpdateAllocationStrategy_SPOT_CAPACITY_OPTIMIZED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CRUpdateAllocationStrategy = CRUpdateAllocationStrategy'
  { fromCRUpdateAllocationStrategy ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern CRUpdateAllocationStrategy_BEST_FIT_PROGRESSIVE :: CRUpdateAllocationStrategy
pattern CRUpdateAllocationStrategy_BEST_FIT_PROGRESSIVE = CRUpdateAllocationStrategy' "BEST_FIT_PROGRESSIVE"

pattern CRUpdateAllocationStrategy_SPOT_CAPACITY_OPTIMIZED :: CRUpdateAllocationStrategy
pattern CRUpdateAllocationStrategy_SPOT_CAPACITY_OPTIMIZED = CRUpdateAllocationStrategy' "SPOT_CAPACITY_OPTIMIZED"

{-# COMPLETE
  CRUpdateAllocationStrategy_BEST_FIT_PROGRESSIVE,
  CRUpdateAllocationStrategy_SPOT_CAPACITY_OPTIMIZED,
  CRUpdateAllocationStrategy'
  #-}
