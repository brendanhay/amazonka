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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkspaceDirectoryType = WorkspaceDirectoryType'
  { fromWorkspaceDirectoryType ::
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

pattern WorkspaceDirectoryType_AD_CONNECTOR :: WorkspaceDirectoryType
pattern WorkspaceDirectoryType_AD_CONNECTOR = WorkspaceDirectoryType' "AD_CONNECTOR"

pattern WorkspaceDirectoryType_SIMPLE_AD :: WorkspaceDirectoryType
pattern WorkspaceDirectoryType_SIMPLE_AD = WorkspaceDirectoryType' "SIMPLE_AD"

{-# COMPLETE
  WorkspaceDirectoryType_AD_CONNECTOR,
  WorkspaceDirectoryType_SIMPLE_AD,
  WorkspaceDirectoryType'
  #-}
