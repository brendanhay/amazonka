{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Rec601Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Rec601Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Rec601 Settings
--
-- /See:/ 'newRec601Settings' smart constructor.
data Rec601Settings = Rec601Settings'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Rec601Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRec601Settings ::
  Rec601Settings
newRec601Settings = Rec601Settings'

instance Core.FromJSON Rec601Settings where
  parseJSON =
    Core.withObject
      "Rec601Settings"
      (\x -> Core.pure Rec601Settings')

instance Core.Hashable Rec601Settings

instance Core.NFData Rec601Settings

instance Core.ToJSON Rec601Settings where
  toJSON = Core.const (Core.Object Core.mempty)
