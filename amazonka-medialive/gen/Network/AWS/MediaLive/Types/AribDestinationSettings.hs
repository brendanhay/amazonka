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
-- Module      : Network.AWS.MediaLive.Types.AribDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AribDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Arib Destination Settings
--
-- /See:/ 'newAribDestinationSettings' smart constructor.
data AribDestinationSettings = AribDestinationSettings'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AribDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAribDestinationSettings ::
  AribDestinationSettings
newAribDestinationSettings = AribDestinationSettings'

instance Core.FromJSON AribDestinationSettings where
  parseJSON =
    Core.withObject
      "AribDestinationSettings"
      (\x -> Core.pure AribDestinationSettings')

instance Core.Hashable AribDestinationSettings

instance Core.NFData AribDestinationSettings

instance Core.ToJSON AribDestinationSettings where
  toJSON = Core.const (Core.Object Core.mempty)
