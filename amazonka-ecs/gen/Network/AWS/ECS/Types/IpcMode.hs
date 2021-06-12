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
-- Module      : Network.AWS.ECS.Types.IpcMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.IpcMode
  ( IpcMode
      ( ..,
        IpcMode_Host,
        IpcMode_None,
        IpcMode_Task
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype IpcMode = IpcMode' {fromIpcMode :: Core.Text}
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern IpcMode_Host :: IpcMode
pattern IpcMode_Host = IpcMode' "host"

pattern IpcMode_None :: IpcMode
pattern IpcMode_None = IpcMode' "none"

pattern IpcMode_Task :: IpcMode
pattern IpcMode_Task = IpcMode' "task"

{-# COMPLETE
  IpcMode_Host,
  IpcMode_None,
  IpcMode_Task,
  IpcMode'
  #-}
