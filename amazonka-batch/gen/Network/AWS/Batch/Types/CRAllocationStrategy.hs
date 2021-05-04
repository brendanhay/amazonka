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
-- Module      : Network.AWS.Batch.Types.CRAllocationStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.CRAllocationStrategy
  ( CRAllocationStrategy
      ( ..,
        CRAllocationStrategy_BEST_FIT,
        CRAllocationStrategy_BEST_FIT_PROGRESSIVE,
        CRAllocationStrategy_SPOT_CAPACITY_OPTIMIZED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CRAllocationStrategy = CRAllocationStrategy'
  { fromCRAllocationStrategy ::
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
