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
-- Module      : Amazonka.ServiceCatalog.Types.RecordStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.RecordStatus
  ( RecordStatus
      ( ..,
        RecordStatus_CREATED,
        RecordStatus_FAILED,
        RecordStatus_IN_PROGRESS,
        RecordStatus_IN_PROGRESS_IN_ERROR,
        RecordStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecordStatus = RecordStatus'
  { fromRecordStatus ::
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

pattern RecordStatus_CREATED :: RecordStatus
pattern RecordStatus_CREATED = RecordStatus' "CREATED"

pattern RecordStatus_FAILED :: RecordStatus
pattern RecordStatus_FAILED = RecordStatus' "FAILED"

pattern RecordStatus_IN_PROGRESS :: RecordStatus
pattern RecordStatus_IN_PROGRESS = RecordStatus' "IN_PROGRESS"

pattern RecordStatus_IN_PROGRESS_IN_ERROR :: RecordStatus
pattern RecordStatus_IN_PROGRESS_IN_ERROR = RecordStatus' "IN_PROGRESS_IN_ERROR"

pattern RecordStatus_SUCCEEDED :: RecordStatus
pattern RecordStatus_SUCCEEDED = RecordStatus' "SUCCEEDED"

{-# COMPLETE
  RecordStatus_CREATED,
  RecordStatus_FAILED,
  RecordStatus_IN_PROGRESS,
  RecordStatus_IN_PROGRESS_IN_ERROR,
  RecordStatus_SUCCEEDED,
  RecordStatus'
  #-}
