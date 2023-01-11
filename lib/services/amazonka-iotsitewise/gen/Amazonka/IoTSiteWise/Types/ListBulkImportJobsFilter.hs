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
-- Module      : Amazonka.IoTSiteWise.Types.ListBulkImportJobsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ListBulkImportJobsFilter
  ( ListBulkImportJobsFilter
      ( ..,
        ListBulkImportJobsFilter_ALL,
        ListBulkImportJobsFilter_CANCELLED,
        ListBulkImportJobsFilter_COMPLETED,
        ListBulkImportJobsFilter_COMPLETED_WITH_FAILURES,
        ListBulkImportJobsFilter_FAILED,
        ListBulkImportJobsFilter_PENDING,
        ListBulkImportJobsFilter_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ListBulkImportJobsFilter = ListBulkImportJobsFilter'
  { fromListBulkImportJobsFilter ::
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

pattern ListBulkImportJobsFilter_ALL :: ListBulkImportJobsFilter
pattern ListBulkImportJobsFilter_ALL = ListBulkImportJobsFilter' "ALL"

pattern ListBulkImportJobsFilter_CANCELLED :: ListBulkImportJobsFilter
pattern ListBulkImportJobsFilter_CANCELLED = ListBulkImportJobsFilter' "CANCELLED"

pattern ListBulkImportJobsFilter_COMPLETED :: ListBulkImportJobsFilter
pattern ListBulkImportJobsFilter_COMPLETED = ListBulkImportJobsFilter' "COMPLETED"

pattern ListBulkImportJobsFilter_COMPLETED_WITH_FAILURES :: ListBulkImportJobsFilter
pattern ListBulkImportJobsFilter_COMPLETED_WITH_FAILURES = ListBulkImportJobsFilter' "COMPLETED_WITH_FAILURES"

pattern ListBulkImportJobsFilter_FAILED :: ListBulkImportJobsFilter
pattern ListBulkImportJobsFilter_FAILED = ListBulkImportJobsFilter' "FAILED"

pattern ListBulkImportJobsFilter_PENDING :: ListBulkImportJobsFilter
pattern ListBulkImportJobsFilter_PENDING = ListBulkImportJobsFilter' "PENDING"

pattern ListBulkImportJobsFilter_RUNNING :: ListBulkImportJobsFilter
pattern ListBulkImportJobsFilter_RUNNING = ListBulkImportJobsFilter' "RUNNING"

{-# COMPLETE
  ListBulkImportJobsFilter_ALL,
  ListBulkImportJobsFilter_CANCELLED,
  ListBulkImportJobsFilter_COMPLETED,
  ListBulkImportJobsFilter_COMPLETED_WITH_FAILURES,
  ListBulkImportJobsFilter_FAILED,
  ListBulkImportJobsFilter_PENDING,
  ListBulkImportJobsFilter_RUNNING,
  ListBulkImportJobsFilter'
  #-}
