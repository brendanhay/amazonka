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
-- Module      : Amazonka.TimeStreamWrite.Types.BatchLoadStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.BatchLoadStatus
  ( BatchLoadStatus
      ( ..,
        BatchLoadStatus_CREATED,
        BatchLoadStatus_FAILED,
        BatchLoadStatus_IN_PROGRESS,
        BatchLoadStatus_PENDING_RESUME,
        BatchLoadStatus_PROGRESS_STOPPED,
        BatchLoadStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BatchLoadStatus = BatchLoadStatus'
  { fromBatchLoadStatus ::
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

pattern BatchLoadStatus_CREATED :: BatchLoadStatus
pattern BatchLoadStatus_CREATED = BatchLoadStatus' "CREATED"

pattern BatchLoadStatus_FAILED :: BatchLoadStatus
pattern BatchLoadStatus_FAILED = BatchLoadStatus' "FAILED"

pattern BatchLoadStatus_IN_PROGRESS :: BatchLoadStatus
pattern BatchLoadStatus_IN_PROGRESS = BatchLoadStatus' "IN_PROGRESS"

pattern BatchLoadStatus_PENDING_RESUME :: BatchLoadStatus
pattern BatchLoadStatus_PENDING_RESUME = BatchLoadStatus' "PENDING_RESUME"

pattern BatchLoadStatus_PROGRESS_STOPPED :: BatchLoadStatus
pattern BatchLoadStatus_PROGRESS_STOPPED = BatchLoadStatus' "PROGRESS_STOPPED"

pattern BatchLoadStatus_SUCCEEDED :: BatchLoadStatus
pattern BatchLoadStatus_SUCCEEDED = BatchLoadStatus' "SUCCEEDED"

{-# COMPLETE
  BatchLoadStatus_CREATED,
  BatchLoadStatus_FAILED,
  BatchLoadStatus_IN_PROGRESS,
  BatchLoadStatus_PENDING_RESUME,
  BatchLoadStatus_PROGRESS_STOPPED,
  BatchLoadStatus_SUCCEEDED,
  BatchLoadStatus'
  #-}
