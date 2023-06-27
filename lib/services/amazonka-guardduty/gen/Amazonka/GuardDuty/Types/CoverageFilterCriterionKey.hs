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
-- Module      : Amazonka.GuardDuty.Types.CoverageFilterCriterionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CoverageFilterCriterionKey
  ( CoverageFilterCriterionKey
      ( ..,
        CoverageFilterCriterionKey_ACCOUNT_ID,
        CoverageFilterCriterionKey_ADDON_VERSION,
        CoverageFilterCriterionKey_CLUSTER_NAME,
        CoverageFilterCriterionKey_COVERAGE_STATUS,
        CoverageFilterCriterionKey_RESOURCE_TYPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CoverageFilterCriterionKey = CoverageFilterCriterionKey'
  { fromCoverageFilterCriterionKey ::
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

pattern CoverageFilterCriterionKey_ACCOUNT_ID :: CoverageFilterCriterionKey
pattern CoverageFilterCriterionKey_ACCOUNT_ID = CoverageFilterCriterionKey' "ACCOUNT_ID"

pattern CoverageFilterCriterionKey_ADDON_VERSION :: CoverageFilterCriterionKey
pattern CoverageFilterCriterionKey_ADDON_VERSION = CoverageFilterCriterionKey' "ADDON_VERSION"

pattern CoverageFilterCriterionKey_CLUSTER_NAME :: CoverageFilterCriterionKey
pattern CoverageFilterCriterionKey_CLUSTER_NAME = CoverageFilterCriterionKey' "CLUSTER_NAME"

pattern CoverageFilterCriterionKey_COVERAGE_STATUS :: CoverageFilterCriterionKey
pattern CoverageFilterCriterionKey_COVERAGE_STATUS = CoverageFilterCriterionKey' "COVERAGE_STATUS"

pattern CoverageFilterCriterionKey_RESOURCE_TYPE :: CoverageFilterCriterionKey
pattern CoverageFilterCriterionKey_RESOURCE_TYPE = CoverageFilterCriterionKey' "RESOURCE_TYPE"

{-# COMPLETE
  CoverageFilterCriterionKey_ACCOUNT_ID,
  CoverageFilterCriterionKey_ADDON_VERSION,
  CoverageFilterCriterionKey_CLUSTER_NAME,
  CoverageFilterCriterionKey_COVERAGE_STATUS,
  CoverageFilterCriterionKey_RESOURCE_TYPE,
  CoverageFilterCriterionKey'
  #-}
