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
-- Module      : Network.AWS.MacieV2.Types.ListJobsFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.ListJobsFilterKey
  ( ListJobsFilterKey
      ( ..,
        ListJobsFilterKey_CreatedAt,
        ListJobsFilterKey_JobStatus,
        ListJobsFilterKey_JobType,
        ListJobsFilterKey_Name
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The property to use to filter the results. Valid values are:
newtype ListJobsFilterKey = ListJobsFilterKey'
  { fromListJobsFilterKey ::
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
