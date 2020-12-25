{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryType
  ( DirectoryType
      ( DirectoryType',
        DirectoryTypeSimpleAD,
        DirectoryTypeADConnector,
        DirectoryTypeMicrosoftAD,
        DirectoryTypeSharedMicrosoftAD,
        fromDirectoryType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DirectoryType = DirectoryType'
  { fromDirectoryType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DirectoryTypeSimpleAD :: DirectoryType
pattern DirectoryTypeSimpleAD = DirectoryType' "SimpleAD"

pattern DirectoryTypeADConnector :: DirectoryType
pattern DirectoryTypeADConnector = DirectoryType' "ADConnector"

pattern DirectoryTypeMicrosoftAD :: DirectoryType
pattern DirectoryTypeMicrosoftAD = DirectoryType' "MicrosoftAD"

pattern DirectoryTypeSharedMicrosoftAD :: DirectoryType
pattern DirectoryTypeSharedMicrosoftAD = DirectoryType' "SharedMicrosoftAD"

{-# COMPLETE
  DirectoryTypeSimpleAD,
  DirectoryTypeADConnector,
  DirectoryTypeMicrosoftAD,
  DirectoryTypeSharedMicrosoftAD,
  DirectoryType'
  #-}
