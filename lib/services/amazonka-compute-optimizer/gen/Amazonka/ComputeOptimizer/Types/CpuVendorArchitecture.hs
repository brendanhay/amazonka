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
-- Module      : Amazonka.ComputeOptimizer.Types.CpuVendorArchitecture
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.CpuVendorArchitecture
  ( CpuVendorArchitecture
      ( ..,
        CpuVendorArchitecture_AWS_ARM64,
        CpuVendorArchitecture_CURRENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CpuVendorArchitecture = CpuVendorArchitecture'
  { fromCpuVendorArchitecture ::
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

pattern CpuVendorArchitecture_AWS_ARM64 :: CpuVendorArchitecture
pattern CpuVendorArchitecture_AWS_ARM64 = CpuVendorArchitecture' "AWS_ARM64"

pattern CpuVendorArchitecture_CURRENT :: CpuVendorArchitecture
pattern CpuVendorArchitecture_CURRENT = CpuVendorArchitecture' "CURRENT"

{-# COMPLETE
  CpuVendorArchitecture_AWS_ARM64,
  CpuVendorArchitecture_CURRENT,
  CpuVendorArchitecture'
  #-}
