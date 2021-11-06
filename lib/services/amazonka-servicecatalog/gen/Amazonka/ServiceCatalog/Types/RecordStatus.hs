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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype RecordStatus = RecordStatus'
  { fromRecordStatus ::
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
