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
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
  ( WorkspaceDirectoryState
      ( ..,
        WorkspaceDirectoryState_DEREGISTERED,
        WorkspaceDirectoryState_DEREGISTERING,
        WorkspaceDirectoryState_ERROR,
        WorkspaceDirectoryState_REGISTERED,
        WorkspaceDirectoryState_REGISTERING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype WorkspaceDirectoryState = WorkspaceDirectoryState'
  { fromWorkspaceDirectoryState ::
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

pattern WorkspaceDirectoryState_DEREGISTERED :: WorkspaceDirectoryState
pattern WorkspaceDirectoryState_DEREGISTERED = WorkspaceDirectoryState' "DEREGISTERED"

pattern WorkspaceDirectoryState_DEREGISTERING :: WorkspaceDirectoryState
pattern WorkspaceDirectoryState_DEREGISTERING = WorkspaceDirectoryState' "DEREGISTERING"

pattern WorkspaceDirectoryState_ERROR :: WorkspaceDirectoryState
pattern WorkspaceDirectoryState_ERROR = WorkspaceDirectoryState' "ERROR"

pattern WorkspaceDirectoryState_REGISTERED :: WorkspaceDirectoryState
pattern WorkspaceDirectoryState_REGISTERED = WorkspaceDirectoryState' "REGISTERED"

pattern WorkspaceDirectoryState_REGISTERING :: WorkspaceDirectoryState
pattern WorkspaceDirectoryState_REGISTERING = WorkspaceDirectoryState' "REGISTERING"

{-# COMPLETE
  WorkspaceDirectoryState_DEREGISTERED,
  WorkspaceDirectoryState_DEREGISTERING,
  WorkspaceDirectoryState_ERROR,
  WorkspaceDirectoryState_REGISTERED,
  WorkspaceDirectoryState_REGISTERING,
  WorkspaceDirectoryState'
  #-}
