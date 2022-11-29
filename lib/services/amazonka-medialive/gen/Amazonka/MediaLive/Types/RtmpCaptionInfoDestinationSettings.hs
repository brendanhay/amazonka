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
-- Module      : Amazonka.MediaLive.Types.RtmpCaptionInfoDestinationSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.RtmpCaptionInfoDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Rtmp Caption Info Destination Settings
--
-- /See:/ 'newRtmpCaptionInfoDestinationSettings' smart constructor.
data RtmpCaptionInfoDestinationSettings = RtmpCaptionInfoDestinationSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
          Prelude.pure RtmpCaptionInfoDestinationSettings'
      )

instance
  Prelude.Hashable
    RtmpCaptionInfoDestinationSettings
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    RtmpCaptionInfoDestinationSettings
  where
  rnf _ = ()

instance
  Core.ToJSON
    RtmpCaptionInfoDestinationSettings
  where
  toJSON = Prelude.const (Core.Object Prelude.mempty)
