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
-- Module      : Network.AWS.MediaLive.Types.Rec709Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Rec709Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Rec709 Settings
--
-- /See:/ 'newRec709Settings' smart constructor.
data Rec709Settings = Rec709Settings'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Rec709Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRec709Settings ::
  Rec709Settings
newRec709Settings = Rec709Settings'

instance Core.FromJSON Rec709Settings where
  parseJSON =
    Core.withObject
      "Rec709Settings"
      (\x -> Core.pure Rec709Settings')

instance Core.Hashable Rec709Settings

instance Core.NFData Rec709Settings

instance Core.ToJSON Rec709Settings where
  toJSON = Core.const (Core.Object Core.mempty)
