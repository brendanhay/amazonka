{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CompilationJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CompilationJobStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype CompilationJobStatus = CompilationJobStatus'
  { fromCompilationJobStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
