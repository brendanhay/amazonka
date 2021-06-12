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
-- Module      : Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Rtmp Caption Info Destination Settings
--
-- /See:/ 'newRtmpCaptionInfoDestinationSettings' smart constructor.
data RtmpCaptionInfoDestinationSettings = RtmpCaptionInfoDestinationSettings'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RtmpCaptionInfoDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRtmpCaptionInfoDestinationSettings ::
  RtmpCaptionInfoDestinationSettings
newRtmpCaptionInfoDestinationSettings =
  RtmpCaptionInfoDestinationSettings'

instance
  Core.FromJSON
    RtmpCaptionInfoDestinationSettings
  where
  parseJSON =
    Core.withObject
      "RtmpCaptionInfoDestinationSettings"
      ( \x ->
          Core.pure RtmpCaptionInfoDestinationSettings'
      )

instance
  Core.Hashable
    RtmpCaptionInfoDestinationSettings

instance
  Core.NFData
    RtmpCaptionInfoDestinationSettings

instance
  Core.ToJSON
    RtmpCaptionInfoDestinationSettings
  where
  toJSON = Core.const (Core.Object Core.mempty)
