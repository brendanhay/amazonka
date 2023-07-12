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
-- Module      : Amazonka.MacieV2.Types.ListJobsSortAttributeName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ListJobsSortAttributeName
  ( ListJobsSortAttributeName
      ( ..,
        ListJobsSortAttributeName_CreatedAt,
        ListJobsSortAttributeName_JobStatus,
        ListJobsSortAttributeName_JobType,
        ListJobsSortAttributeName_Name
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The property to sort the results by. Valid values are:
newtype ListJobsSortAttributeName = ListJobsSortAttributeName'
  { fromListJobsSortAttributeName ::
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

pattern ListJobsSortAttributeName_CreatedAt :: ListJobsSortAttributeName
pattern ListJobsSortAttributeName_CreatedAt = ListJobsSortAttributeName' "createdAt"

pattern ListJobsSortAttributeName_JobStatus :: ListJobsSortAttributeName
pattern ListJobsSortAttributeName_JobStatus = ListJobsSortAttributeName' "jobStatus"

pattern ListJobsSortAttributeName_JobType :: ListJobsSortAttributeName
pattern ListJobsSortAttributeName_JobType = ListJobsSortAttributeName' "jobType"

pattern ListJobsSortAttributeName_Name :: ListJobsSortAttributeName
pattern ListJobsSortAttributeName_Name = ListJobsSortAttributeName' "name"

{-# COMPLETE
  ListJobsSortAttributeName_CreatedAt,
  ListJobsSortAttributeName_JobStatus,
  ListJobsSortAttributeName_JobType,
  ListJobsSortAttributeName_Name,
  ListJobsSortAttributeName'
  #-}
