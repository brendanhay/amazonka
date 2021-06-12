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
-- Module      : Network.AWS.MediaLive.Types.TeletextDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TeletextDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Teletext Destination Settings
--
-- /See:/ 'newTeletextDestinationSettings' smart constructor.
data TeletextDestinationSettings = TeletextDestinationSettings'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TeletextDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTeletextDestinationSettings ::
  TeletextDestinationSettings
newTeletextDestinationSettings =
  TeletextDestinationSettings'

instance Core.FromJSON TeletextDestinationSettings where
  parseJSON =
    Core.withObject
      "TeletextDestinationSettings"
      (\x -> Core.pure TeletextDestinationSettings')

instance Core.Hashable TeletextDestinationSettings

instance Core.NFData TeletextDestinationSettings

instance Core.ToJSON TeletextDestinationSettings where
  toJSON = Core.const (Core.Object Core.mempty)
