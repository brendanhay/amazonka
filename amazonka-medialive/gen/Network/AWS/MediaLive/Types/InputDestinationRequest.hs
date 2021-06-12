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
-- Module      : Network.AWS.MediaLive.Types.InputDestinationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDestinationRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Endpoint settings for a PUSH type input.
--
-- /See:/ 'newInputDestinationRequest' smart constructor.
data InputDestinationRequest = InputDestinationRequest'
  { -- | A unique name for the location the RTMP stream is being pushed to.
    streamName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputDestinationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamName', 'inputDestinationRequest_streamName' - A unique name for the location the RTMP stream is being pushed to.
newInputDestinationRequest ::
  InputDestinationRequest
newInputDestinationRequest =
  InputDestinationRequest' {streamName = Core.Nothing}

-- | A unique name for the location the RTMP stream is being pushed to.
inputDestinationRequest_streamName :: Lens.Lens' InputDestinationRequest (Core.Maybe Core.Text)
inputDestinationRequest_streamName = Lens.lens (\InputDestinationRequest' {streamName} -> streamName) (\s@InputDestinationRequest' {} a -> s {streamName = a} :: InputDestinationRequest)

instance Core.Hashable InputDestinationRequest

instance Core.NFData InputDestinationRequest

instance Core.ToJSON InputDestinationRequest where
  toJSON InputDestinationRequest' {..} =
    Core.object
      ( Core.catMaybes
          [("streamName" Core..=) Core.<$> streamName]
      )
