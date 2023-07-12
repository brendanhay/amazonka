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
-- Module      : Amazonka.WorkSpaces.Types.WorkspaceDirectoryState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceDirectoryState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkspaceDirectoryState = WorkspaceDirectoryState'
  { fromWorkspaceDirectoryState ::
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
