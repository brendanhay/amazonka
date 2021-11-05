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
-- Module      : Amazonka.AMP.Types.WorkspaceStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.WorkspaceStatusCode
  ( WorkspaceStatusCode
      ( ..,
        WorkspaceStatusCode_ACTIVE,
        WorkspaceStatusCode_CREATING,
        WorkspaceStatusCode_CREATION_FAILED,
        WorkspaceStatusCode_DELETING,
        WorkspaceStatusCode_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | State of a workspace.
newtype WorkspaceStatusCode = WorkspaceStatusCode'
  { fromWorkspaceStatusCode ::
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

pattern WorkspaceStatusCode_ACTIVE :: WorkspaceStatusCode
pattern WorkspaceStatusCode_ACTIVE = WorkspaceStatusCode' "ACTIVE"

pattern WorkspaceStatusCode_CREATING :: WorkspaceStatusCode
pattern WorkspaceStatusCode_CREATING = WorkspaceStatusCode' "CREATING"

pattern WorkspaceStatusCode_CREATION_FAILED :: WorkspaceStatusCode
pattern WorkspaceStatusCode_CREATION_FAILED = WorkspaceStatusCode' "CREATION_FAILED"

pattern WorkspaceStatusCode_DELETING :: WorkspaceStatusCode
pattern WorkspaceStatusCode_DELETING = WorkspaceStatusCode' "DELETING"

pattern WorkspaceStatusCode_UPDATING :: WorkspaceStatusCode
pattern WorkspaceStatusCode_UPDATING = WorkspaceStatusCode' "UPDATING"

{-# COMPLETE
  WorkspaceStatusCode_ACTIVE,
  WorkspaceStatusCode_CREATING,
  WorkspaceStatusCode_CREATION_FAILED,
  WorkspaceStatusCode_DELETING,
  WorkspaceStatusCode_UPDATING,
  WorkspaceStatusCode'
  #-}
