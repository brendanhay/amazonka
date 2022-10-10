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
-- Module      : Amazonka.WorkSpaces.Types.WorkspaceDirectoryType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceDirectoryType
  ( WorkspaceDirectoryType
      ( ..,
        WorkspaceDirectoryType_AD_CONNECTOR,
        WorkspaceDirectoryType_SIMPLE_AD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype WorkspaceDirectoryType = WorkspaceDirectoryType'
  { fromWorkspaceDirectoryType ::
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

pattern WorkspaceDirectoryType_AD_CONNECTOR :: WorkspaceDirectoryType
pattern WorkspaceDirectoryType_AD_CONNECTOR = WorkspaceDirectoryType' "AD_CONNECTOR"

pattern WorkspaceDirectoryType_SIMPLE_AD :: WorkspaceDirectoryType
pattern WorkspaceDirectoryType_SIMPLE_AD = WorkspaceDirectoryType' "SIMPLE_AD"

{-# COMPLETE
  WorkspaceDirectoryType_AD_CONNECTOR,
  WorkspaceDirectoryType_SIMPLE_AD,
  WorkspaceDirectoryType'
  #-}
