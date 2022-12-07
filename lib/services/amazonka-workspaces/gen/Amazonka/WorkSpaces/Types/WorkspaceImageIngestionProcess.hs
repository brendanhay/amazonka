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
-- Module      : Amazonka.WorkSpaces.Types.WorkspaceImageIngestionProcess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceImageIngestionProcess
  ( WorkspaceImageIngestionProcess
      ( ..,
        WorkspaceImageIngestionProcess_BYOL_GRAPHICS,
        WorkspaceImageIngestionProcess_BYOL_GRAPHICSPRO,
        WorkspaceImageIngestionProcess_BYOL_GRAPHICS_G4DN,
        WorkspaceImageIngestionProcess_BYOL_GRAPHICS_G4DN_BYOP,
        WorkspaceImageIngestionProcess_BYOL_REGULAR,
        WorkspaceImageIngestionProcess_BYOL_REGULAR_BYOP,
        WorkspaceImageIngestionProcess_BYOL_REGULAR_WSP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkspaceImageIngestionProcess = WorkspaceImageIngestionProcess'
  { fromWorkspaceImageIngestionProcess ::
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

pattern WorkspaceImageIngestionProcess_BYOL_GRAPHICS :: WorkspaceImageIngestionProcess
pattern WorkspaceImageIngestionProcess_BYOL_GRAPHICS = WorkspaceImageIngestionProcess' "BYOL_GRAPHICS"

pattern WorkspaceImageIngestionProcess_BYOL_GRAPHICSPRO :: WorkspaceImageIngestionProcess
pattern WorkspaceImageIngestionProcess_BYOL_GRAPHICSPRO = WorkspaceImageIngestionProcess' "BYOL_GRAPHICSPRO"

pattern WorkspaceImageIngestionProcess_BYOL_GRAPHICS_G4DN :: WorkspaceImageIngestionProcess
pattern WorkspaceImageIngestionProcess_BYOL_GRAPHICS_G4DN = WorkspaceImageIngestionProcess' "BYOL_GRAPHICS_G4DN"

pattern WorkspaceImageIngestionProcess_BYOL_GRAPHICS_G4DN_BYOP :: WorkspaceImageIngestionProcess
pattern WorkspaceImageIngestionProcess_BYOL_GRAPHICS_G4DN_BYOP = WorkspaceImageIngestionProcess' "BYOL_GRAPHICS_G4DN_BYOP"

pattern WorkspaceImageIngestionProcess_BYOL_REGULAR :: WorkspaceImageIngestionProcess
pattern WorkspaceImageIngestionProcess_BYOL_REGULAR = WorkspaceImageIngestionProcess' "BYOL_REGULAR"

pattern WorkspaceImageIngestionProcess_BYOL_REGULAR_BYOP :: WorkspaceImageIngestionProcess
pattern WorkspaceImageIngestionProcess_BYOL_REGULAR_BYOP = WorkspaceImageIngestionProcess' "BYOL_REGULAR_BYOP"

pattern WorkspaceImageIngestionProcess_BYOL_REGULAR_WSP :: WorkspaceImageIngestionProcess
pattern WorkspaceImageIngestionProcess_BYOL_REGULAR_WSP = WorkspaceImageIngestionProcess' "BYOL_REGULAR_WSP"

{-# COMPLETE
  WorkspaceImageIngestionProcess_BYOL_GRAPHICS,
  WorkspaceImageIngestionProcess_BYOL_GRAPHICSPRO,
  WorkspaceImageIngestionProcess_BYOL_GRAPHICS_G4DN,
  WorkspaceImageIngestionProcess_BYOL_GRAPHICS_G4DN_BYOP,
  WorkspaceImageIngestionProcess_BYOL_REGULAR,
  WorkspaceImageIngestionProcess_BYOL_REGULAR_BYOP,
  WorkspaceImageIngestionProcess_BYOL_REGULAR_WSP,
  WorkspaceImageIngestionProcess'
  #-}
