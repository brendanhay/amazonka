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
-- Module      : Amazonka.LookoutVision.Types.ModelPackagingJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ModelPackagingJobStatus
  ( ModelPackagingJobStatus
      ( ..,
        ModelPackagingJobStatus_CREATED,
        ModelPackagingJobStatus_FAILED,
        ModelPackagingJobStatus_RUNNING,
        ModelPackagingJobStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelPackagingJobStatus = ModelPackagingJobStatus'
  { fromModelPackagingJobStatus ::
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

pattern ModelPackagingJobStatus_CREATED :: ModelPackagingJobStatus
pattern ModelPackagingJobStatus_CREATED = ModelPackagingJobStatus' "CREATED"

pattern ModelPackagingJobStatus_FAILED :: ModelPackagingJobStatus
pattern ModelPackagingJobStatus_FAILED = ModelPackagingJobStatus' "FAILED"

pattern ModelPackagingJobStatus_RUNNING :: ModelPackagingJobStatus
pattern ModelPackagingJobStatus_RUNNING = ModelPackagingJobStatus' "RUNNING"

pattern ModelPackagingJobStatus_SUCCEEDED :: ModelPackagingJobStatus
pattern ModelPackagingJobStatus_SUCCEEDED = ModelPackagingJobStatus' "SUCCEEDED"

{-# COMPLETE
  ModelPackagingJobStatus_CREATED,
  ModelPackagingJobStatus_FAILED,
  ModelPackagingJobStatus_RUNNING,
  ModelPackagingJobStatus_SUCCEEDED,
  ModelPackagingJobStatus'
  #-}
