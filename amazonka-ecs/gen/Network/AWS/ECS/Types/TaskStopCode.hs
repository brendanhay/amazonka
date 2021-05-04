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
-- Module      : Network.AWS.ECS.Types.TaskStopCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskStopCode
  ( TaskStopCode
      ( ..,
        TaskStopCode_EssentialContainerExited,
        TaskStopCode_TaskFailedToStart,
        TaskStopCode_UserInitiated
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TaskStopCode = TaskStopCode'
  { fromTaskStopCode ::
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
