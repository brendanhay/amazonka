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
-- Module      : Network.AWS.MacieV2.Types.ListJobsSortAttributeName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.ListJobsSortAttributeName
  ( ListJobsSortAttributeName
      ( ..,
        ListJobsSortAttributeName_CreatedAt,
        ListJobsSortAttributeName_JobStatus,
        ListJobsSortAttributeName_JobType,
        ListJobsSortAttributeName_Name
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The property to sort the results by. Valid values are:
newtype ListJobsSortAttributeName = ListJobsSortAttributeName'
  { fromListJobsSortAttributeName ::
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
