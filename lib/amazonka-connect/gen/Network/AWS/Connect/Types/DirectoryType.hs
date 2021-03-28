{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.DirectoryType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.DirectoryType
  ( DirectoryType
    ( DirectoryType'
    , DirectoryTypeSaml
    , DirectoryTypeConnectManaged
    , DirectoryTypeExistingDirectory
    , fromDirectoryType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DirectoryType = DirectoryType'{fromDirectoryType ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern DirectoryTypeSaml :: DirectoryType
pattern DirectoryTypeSaml = DirectoryType' "SAML"

pattern DirectoryTypeConnectManaged :: DirectoryType
pattern DirectoryTypeConnectManaged = DirectoryType' "CONNECT_MANAGED"

pattern DirectoryTypeExistingDirectory :: DirectoryType
pattern DirectoryTypeExistingDirectory = DirectoryType' "EXISTING_DIRECTORY"

{-# COMPLETE 
  DirectoryTypeSaml,

  DirectoryTypeConnectManaged,

  DirectoryTypeExistingDirectory,
  DirectoryType'
  #-}
