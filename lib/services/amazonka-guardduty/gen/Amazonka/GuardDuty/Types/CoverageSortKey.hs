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
-- Module      : Amazonka.GuardDuty.Types.CoverageSortKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CoverageSortKey
  ( CoverageSortKey
      ( ..,
        CoverageSortKey_ACCOUNT_ID,
        CoverageSortKey_ADDON_VERSION,
        CoverageSortKey_CLUSTER_NAME,
        CoverageSortKey_COVERAGE_STATUS,
        CoverageSortKey_ISSUE,
        CoverageSortKey_UPDATED_AT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CoverageSortKey = CoverageSortKey'
  { fromCoverageSortKey ::
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

pattern CoverageSortKey_ACCOUNT_ID :: CoverageSortKey
pattern CoverageSortKey_ACCOUNT_ID = CoverageSortKey' "ACCOUNT_ID"

pattern CoverageSortKey_ADDON_VERSION :: CoverageSortKey
pattern CoverageSortKey_ADDON_VERSION = CoverageSortKey' "ADDON_VERSION"

pattern CoverageSortKey_CLUSTER_NAME :: CoverageSortKey
pattern CoverageSortKey_CLUSTER_NAME = CoverageSortKey' "CLUSTER_NAME"

pattern CoverageSortKey_COVERAGE_STATUS :: CoverageSortKey
pattern CoverageSortKey_COVERAGE_STATUS = CoverageSortKey' "COVERAGE_STATUS"

pattern CoverageSortKey_ISSUE :: CoverageSortKey
pattern CoverageSortKey_ISSUE = CoverageSortKey' "ISSUE"

pattern CoverageSortKey_UPDATED_AT :: CoverageSortKey
pattern CoverageSortKey_UPDATED_AT = CoverageSortKey' "UPDATED_AT"

{-# COMPLETE
  CoverageSortKey_ACCOUNT_ID,
  CoverageSortKey_ADDON_VERSION,
  CoverageSortKey_CLUSTER_NAME,
  CoverageSortKey_COVERAGE_STATUS,
  CoverageSortKey_ISSUE,
  CoverageSortKey_UPDATED_AT,
  CoverageSortKey'
  #-}
