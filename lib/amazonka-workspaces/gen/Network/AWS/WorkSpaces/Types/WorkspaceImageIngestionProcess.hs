{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceImageIngestionProcess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceImageIngestionProcess
  ( WorkspaceImageIngestionProcess
      ( WorkspaceImageIngestionProcess',
        WorkspaceImageIngestionProcessByolRegular,
        WorkspaceImageIngestionProcessByolGraphics,
        WorkspaceImageIngestionProcessByolGraphicspro,
        fromWorkspaceImageIngestionProcess
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype WorkspaceImageIngestionProcess = WorkspaceImageIngestionProcess'
  { fromWorkspaceImageIngestionProcess ::
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

pattern WorkspaceImageIngestionProcessByolRegular :: WorkspaceImageIngestionProcess
pattern WorkspaceImageIngestionProcessByolRegular = WorkspaceImageIngestionProcess' "BYOL_REGULAR"

pattern WorkspaceImageIngestionProcessByolGraphics :: WorkspaceImageIngestionProcess
pattern WorkspaceImageIngestionProcessByolGraphics = WorkspaceImageIngestionProcess' "BYOL_GRAPHICS"

pattern WorkspaceImageIngestionProcessByolGraphicspro :: WorkspaceImageIngestionProcess
pattern WorkspaceImageIngestionProcessByolGraphicspro = WorkspaceImageIngestionProcess' "BYOL_GRAPHICSPRO"

{-# COMPLETE
  WorkspaceImageIngestionProcessByolRegular,
  WorkspaceImageIngestionProcessByolGraphics,
  WorkspaceImageIngestionProcessByolGraphicspro,
  WorkspaceImageIngestionProcess'
  #-}
