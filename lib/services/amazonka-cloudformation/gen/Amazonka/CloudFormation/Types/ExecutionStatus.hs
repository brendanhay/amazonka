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
-- Module      : Amazonka.CloudFormation.Types.ExecutionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ExecutionStatus
  ( ExecutionStatus
      ( ..,
        ExecutionStatus_AVAILABLE,
        ExecutionStatus_EXECUTE_COMPLETE,
        ExecutionStatus_EXECUTE_FAILED,
        ExecutionStatus_EXECUTE_IN_PROGRESS,
        ExecutionStatus_OBSOLETE,
        ExecutionStatus_UNAVAILABLE
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

pattern ExecutionStatus_AVAILABLE :: ExecutionStatus
pattern ExecutionStatus_AVAILABLE = ExecutionStatus' "AVAILABLE"

pattern ExecutionStatus_EXECUTE_COMPLETE :: ExecutionStatus
pattern ExecutionStatus_EXECUTE_COMPLETE = ExecutionStatus' "EXECUTE_COMPLETE"

pattern ExecutionStatus_EXECUTE_FAILED :: ExecutionStatus
pattern ExecutionStatus_EXECUTE_FAILED = ExecutionStatus' "EXECUTE_FAILED"

pattern ExecutionStatus_EXECUTE_IN_PROGRESS :: ExecutionStatus
pattern ExecutionStatus_EXECUTE_IN_PROGRESS = ExecutionStatus' "EXECUTE_IN_PROGRESS"

pattern ExecutionStatus_OBSOLETE :: ExecutionStatus
pattern ExecutionStatus_OBSOLETE = ExecutionStatus' "OBSOLETE"

pattern ExecutionStatus_UNAVAILABLE :: ExecutionStatus
pattern ExecutionStatus_UNAVAILABLE = ExecutionStatus' "UNAVAILABLE"

{-# COMPLETE
  ExecutionStatus_AVAILABLE,
  ExecutionStatus_EXECUTE_COMPLETE,
  ExecutionStatus_EXECUTE_FAILED,
  ExecutionStatus_EXECUTE_IN_PROGRESS,
  ExecutionStatus_OBSOLETE,
  ExecutionStatus_UNAVAILABLE,
  ExecutionStatus'
  #-}
