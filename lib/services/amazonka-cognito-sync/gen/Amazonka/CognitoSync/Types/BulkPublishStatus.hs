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
-- Module      : Amazonka.CognitoSync.Types.BulkPublishStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Types.BulkPublishStatus
  ( BulkPublishStatus
      ( ..,
        BulkPublishStatus_FAILED,
        BulkPublishStatus_IN_PROGRESS,
        BulkPublishStatus_NOT_STARTED,
        BulkPublishStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BulkPublishStatus = BulkPublishStatus'
  { fromBulkPublishStatus ::
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

pattern BulkPublishStatus_FAILED :: BulkPublishStatus
pattern BulkPublishStatus_FAILED = BulkPublishStatus' "FAILED"

pattern BulkPublishStatus_IN_PROGRESS :: BulkPublishStatus
pattern BulkPublishStatus_IN_PROGRESS = BulkPublishStatus' "IN_PROGRESS"

pattern BulkPublishStatus_NOT_STARTED :: BulkPublishStatus
pattern BulkPublishStatus_NOT_STARTED = BulkPublishStatus' "NOT_STARTED"

pattern BulkPublishStatus_SUCCEEDED :: BulkPublishStatus
pattern BulkPublishStatus_SUCCEEDED = BulkPublishStatus' "SUCCEEDED"

{-# COMPLETE
  BulkPublishStatus_FAILED,
  BulkPublishStatus_IN_PROGRESS,
  BulkPublishStatus_NOT_STARTED,
  BulkPublishStatus_SUCCEEDED,
  BulkPublishStatus'
  #-}
