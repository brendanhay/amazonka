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
-- Module      : Network.AWS.MediaLive.Types.MediaConnectFlowRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaConnectFlowRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The settings for a MediaConnect Flow.
--
-- /See:/ 'newMediaConnectFlowRequest' smart constructor.
data MediaConnectFlowRequest = MediaConnectFlowRequest'
  { -- | The ARN of the MediaConnect Flow that you want to use as a source.
    flowArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MediaConnectFlowRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'mediaConnectFlowRequest_flowArn' - The ARN of the MediaConnect Flow that you want to use as a source.
newMediaConnectFlowRequest ::
  MediaConnectFlowRequest
newMediaConnectFlowRequest =
  MediaConnectFlowRequest' {flowArn = Core.Nothing}

-- | The ARN of the MediaConnect Flow that you want to use as a source.
mediaConnectFlowRequest_flowArn :: Lens.Lens' MediaConnectFlowRequest (Core.Maybe Core.Text)
mediaConnectFlowRequest_flowArn = Lens.lens (\MediaConnectFlowRequest' {flowArn} -> flowArn) (\s@MediaConnectFlowRequest' {} a -> s {flowArn = a} :: MediaConnectFlowRequest)

instance Core.Hashable MediaConnectFlowRequest

instance Core.NFData MediaConnectFlowRequest

instance Core.ToJSON MediaConnectFlowRequest where
  toJSON MediaConnectFlowRequest' {..} =
    Core.object
      ( Core.catMaybes
          [("flowArn" Core..=) Core.<$> flowArn]
      )
