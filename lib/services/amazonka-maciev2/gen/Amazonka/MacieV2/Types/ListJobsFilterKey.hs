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
-- Module      : Amazonka.MacieV2.Types.ListJobsFilterKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ListJobsFilterKey
  ( ListJobsFilterKey
      ( ..,
        ListJobsFilterKey_CreatedAt,
        ListJobsFilterKey_JobStatus,
        ListJobsFilterKey_JobType,
        ListJobsFilterKey_Name
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The property to use to filter the results. Valid values are:
newtype ListJobsFilterKey = ListJobsFilterKey'
  { fromListJobsFilterKey ::
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

pattern ListJobsFilterKey_CreatedAt :: ListJobsFilterKey
pattern ListJobsFilterKey_CreatedAt = ListJobsFilterKey' "createdAt"

pattern ListJobsFilterKey_JobStatus :: ListJobsFilterKey
pattern ListJobsFilterKey_JobStatus = ListJobsFilterKey' "jobStatus"

pattern ListJobsFilterKey_JobType :: ListJobsFilterKey
pattern ListJobsFilterKey_JobType = ListJobsFilterKey' "jobType"

pattern ListJobsFilterKey_Name :: ListJobsFilterKey
pattern ListJobsFilterKey_Name = ListJobsFilterKey' "name"

{-# COMPLETE
  ListJobsFilterKey_CreatedAt,
  ListJobsFilterKey_JobStatus,
  ListJobsFilterKey_JobType,
  ListJobsFilterKey_Name,
  ListJobsFilterKey'
  #-}
