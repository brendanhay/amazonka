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
-- Module      : Amazonka.IAM.Types.JobStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.JobStatusType
  ( JobStatusType
      ( ..,
        JobStatusType_COMPLETED,
        JobStatusType_FAILED,
        JobStatusType_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype JobStatusType = JobStatusType'
  { fromJobStatusType ::
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

pattern JobStatusType_COMPLETED :: JobStatusType
pattern JobStatusType_COMPLETED = JobStatusType' "COMPLETED"

pattern JobStatusType_FAILED :: JobStatusType
pattern JobStatusType_FAILED = JobStatusType' "FAILED"

pattern JobStatusType_IN_PROGRESS :: JobStatusType
pattern JobStatusType_IN_PROGRESS = JobStatusType' "IN_PROGRESS"

{-# COMPLETE
  JobStatusType_COMPLETED,
  JobStatusType_FAILED,
  JobStatusType_IN_PROGRESS,
  JobStatusType'
  #-}
