{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceImageIngestionProcess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceImageIngestionProcess where

import Network.AWS.Prelude

data WorkspaceImageIngestionProcess
  = ByolGraphics
  | ByolGraphicspro
  | ByolRegular
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText WorkspaceImageIngestionProcess where
  parser =
    takeLowerText >>= \case
      "byol_graphics" -> pure ByolGraphics
      "byol_graphicspro" -> pure ByolGraphicspro
      "byol_regular" -> pure ByolRegular
      e ->
        fromTextError $
          "Failure parsing WorkspaceImageIngestionProcess from value: '" <> e
            <> "'. Accepted values: byol_graphics, byol_graphicspro, byol_regular"

instance ToText WorkspaceImageIngestionProcess where
  toText = \case
    ByolGraphics -> "BYOL_GRAPHICS"
    ByolGraphicspro -> "BYOL_GRAPHICSPRO"
    ByolRegular -> "BYOL_REGULAR"

instance Hashable WorkspaceImageIngestionProcess

instance NFData WorkspaceImageIngestionProcess

instance ToByteString WorkspaceImageIngestionProcess

instance ToQuery WorkspaceImageIngestionProcess

instance ToHeader WorkspaceImageIngestionProcess

instance ToJSON WorkspaceImageIngestionProcess where
  toJSON = toJSONText
