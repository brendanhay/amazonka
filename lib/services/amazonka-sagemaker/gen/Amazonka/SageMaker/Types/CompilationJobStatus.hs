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
-- Module      : Amazonka.SageMaker.Types.CompilationJobStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CompilationJobStatus
  ( CompilationJobStatus
      ( ..,
        CompilationJobStatus_COMPLETED,
        CompilationJobStatus_FAILED,
        CompilationJobStatus_INPROGRESS,
        CompilationJobStatus_STARTING,
        CompilationJobStatus_STOPPED,
        CompilationJobStatus_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CompilationJobStatus = CompilationJobStatus'
  { fromCompilationJobStatus ::
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

pattern CompilationJobStatus_COMPLETED :: CompilationJobStatus
pattern CompilationJobStatus_COMPLETED = CompilationJobStatus' "COMPLETED"

pattern CompilationJobStatus_FAILED :: CompilationJobStatus
pattern CompilationJobStatus_FAILED = CompilationJobStatus' "FAILED"

pattern CompilationJobStatus_INPROGRESS :: CompilationJobStatus
pattern CompilationJobStatus_INPROGRESS = CompilationJobStatus' "INPROGRESS"

pattern CompilationJobStatus_STARTING :: CompilationJobStatus
pattern CompilationJobStatus_STARTING = CompilationJobStatus' "STARTING"

pattern CompilationJobStatus_STOPPED :: CompilationJobStatus
pattern CompilationJobStatus_STOPPED = CompilationJobStatus' "STOPPED"

pattern CompilationJobStatus_STOPPING :: CompilationJobStatus
pattern CompilationJobStatus_STOPPING = CompilationJobStatus' "STOPPING"

{-# COMPLETE
  CompilationJobStatus_COMPLETED,
  CompilationJobStatus_FAILED,
  CompilationJobStatus_INPROGRESS,
  CompilationJobStatus_STARTING,
  CompilationJobStatus_STOPPED,
  CompilationJobStatus_STOPPING,
  CompilationJobStatus'
  #-}
