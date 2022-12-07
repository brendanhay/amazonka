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
-- Module      : Amazonka.ECS.Types.TaskStopCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.TaskStopCode
  ( TaskStopCode
      ( ..,
        TaskStopCode_EssentialContainerExited,
        TaskStopCode_TaskFailedToStart,
        TaskStopCode_UserInitiated
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TaskStopCode = TaskStopCode'
  { fromTaskStopCode ::
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

pattern TaskStopCode_EssentialContainerExited :: TaskStopCode
pattern TaskStopCode_EssentialContainerExited = TaskStopCode' "EssentialContainerExited"

pattern TaskStopCode_TaskFailedToStart :: TaskStopCode
pattern TaskStopCode_TaskFailedToStart = TaskStopCode' "TaskFailedToStart"

pattern TaskStopCode_UserInitiated :: TaskStopCode
pattern TaskStopCode_UserInitiated = TaskStopCode' "UserInitiated"

{-# COMPLETE
  TaskStopCode_EssentialContainerExited,
  TaskStopCode_TaskFailedToStart,
  TaskStopCode_UserInitiated,
  TaskStopCode'
  #-}
