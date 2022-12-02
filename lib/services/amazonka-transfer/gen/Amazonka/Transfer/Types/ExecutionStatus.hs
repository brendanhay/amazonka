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
-- Module      : Amazonka.Transfer.Types.ExecutionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ExecutionStatus
  ( ExecutionStatus
      ( ..,
        ExecutionStatus_COMPLETED,
        ExecutionStatus_EXCEPTION,
        ExecutionStatus_HANDLING_EXCEPTION,
        ExecutionStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExecutionStatus = ExecutionStatus'
  { fromExecutionStatus ::
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

pattern ExecutionStatus_COMPLETED :: ExecutionStatus
pattern ExecutionStatus_COMPLETED = ExecutionStatus' "COMPLETED"

pattern ExecutionStatus_EXCEPTION :: ExecutionStatus
pattern ExecutionStatus_EXCEPTION = ExecutionStatus' "EXCEPTION"

pattern ExecutionStatus_HANDLING_EXCEPTION :: ExecutionStatus
pattern ExecutionStatus_HANDLING_EXCEPTION = ExecutionStatus' "HANDLING_EXCEPTION"

pattern ExecutionStatus_IN_PROGRESS :: ExecutionStatus
pattern ExecutionStatus_IN_PROGRESS = ExecutionStatus' "IN_PROGRESS"

{-# COMPLETE
  ExecutionStatus_COMPLETED,
  ExecutionStatus_EXCEPTION,
  ExecutionStatus_HANDLING_EXCEPTION,
  ExecutionStatus_IN_PROGRESS,
  ExecutionStatus'
  #-}
