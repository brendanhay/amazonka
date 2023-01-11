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
-- Module      : Amazonka.GreengrassV2.Types.IoTJobExecutionFailureType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.IoTJobExecutionFailureType
  ( IoTJobExecutionFailureType
      ( ..,
        IoTJobExecutionFailureType_ALL,
        IoTJobExecutionFailureType_FAILED,
        IoTJobExecutionFailureType_REJECTED,
        IoTJobExecutionFailureType_TIMED_OUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IoTJobExecutionFailureType = IoTJobExecutionFailureType'
  { fromIoTJobExecutionFailureType ::
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

pattern IoTJobExecutionFailureType_ALL :: IoTJobExecutionFailureType
pattern IoTJobExecutionFailureType_ALL = IoTJobExecutionFailureType' "ALL"

pattern IoTJobExecutionFailureType_FAILED :: IoTJobExecutionFailureType
pattern IoTJobExecutionFailureType_FAILED = IoTJobExecutionFailureType' "FAILED"

pattern IoTJobExecutionFailureType_REJECTED :: IoTJobExecutionFailureType
pattern IoTJobExecutionFailureType_REJECTED = IoTJobExecutionFailureType' "REJECTED"

pattern IoTJobExecutionFailureType_TIMED_OUT :: IoTJobExecutionFailureType
pattern IoTJobExecutionFailureType_TIMED_OUT = IoTJobExecutionFailureType' "TIMED_OUT"

{-# COMPLETE
  IoTJobExecutionFailureType_ALL,
  IoTJobExecutionFailureType_FAILED,
  IoTJobExecutionFailureType_REJECTED,
  IoTJobExecutionFailureType_TIMED_OUT,
  IoTJobExecutionFailureType'
  #-}
