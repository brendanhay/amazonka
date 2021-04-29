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
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType
  ( WorkspaceDirectoryType
      ( ..,
        WorkspaceDirectoryType_AD_CONNECTOR,
        WorkspaceDirectoryType_SIMPLE_AD
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype WorkspaceDirectoryType = WorkspaceDirectoryType'
  { fromWorkspaceDirectoryType ::
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

pattern WorkspaceDirectoryType_AD_CONNECTOR :: WorkspaceDirectoryType
pattern WorkspaceDirectoryType_AD_CONNECTOR = WorkspaceDirectoryType' "AD_CONNECTOR"

pattern WorkspaceDirectoryType_SIMPLE_AD :: WorkspaceDirectoryType
pattern WorkspaceDirectoryType_SIMPLE_AD = WorkspaceDirectoryType' "SIMPLE_AD"

{-# COMPLETE
  WorkspaceDirectoryType_AD_CONNECTOR,
  WorkspaceDirectoryType_SIMPLE_AD,
  WorkspaceDirectoryType'
  #-}
