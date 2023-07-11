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
-- Module      : Amazonka.Batch.Types.CRAllocationStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.CRAllocationStrategy
  ( CRAllocationStrategy
      ( ..,
        CRAllocationStrategy_BEST_FIT,
        CRAllocationStrategy_BEST_FIT_PROGRESSIVE,
        CRAllocationStrategy_SPOT_CAPACITY_OPTIMIZED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CRAllocationStrategy = CRAllocationStrategy'
  { fromCRAllocationStrategy ::
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

pattern CRAllocationStrategy_BEST_FIT :: CRAllocationStrategy
pattern CRAllocationStrategy_BEST_FIT = CRAllocationStrategy' "BEST_FIT"

pattern CRAllocationStrategy_BEST_FIT_PROGRESSIVE :: CRAllocationStrategy
pattern CRAllocationStrategy_BEST_FIT_PROGRESSIVE = CRAllocationStrategy' "BEST_FIT_PROGRESSIVE"

pattern CRAllocationStrategy_SPOT_CAPACITY_OPTIMIZED :: CRAllocationStrategy
pattern CRAllocationStrategy_SPOT_CAPACITY_OPTIMIZED = CRAllocationStrategy' "SPOT_CAPACITY_OPTIMIZED"

{-# COMPLETE
  CRAllocationStrategy_BEST_FIT,
  CRAllocationStrategy_BEST_FIT_PROGRESSIVE,
  CRAllocationStrategy_SPOT_CAPACITY_OPTIMIZED,
  CRAllocationStrategy'
  #-}
