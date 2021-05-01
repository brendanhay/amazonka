{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureHlsSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureHlsSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Frame Capture Hls Settings
--
-- /See:/ 'newFrameCaptureHlsSettings' smart constructor.
data FrameCaptureHlsSettings = FrameCaptureHlsSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FrameCaptureHlsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newFrameCaptureHlsSettings ::
  FrameCaptureHlsSettings
newFrameCaptureHlsSettings = FrameCaptureHlsSettings'

instance Prelude.FromJSON FrameCaptureHlsSettings where
  parseJSON =
    Prelude.withObject
      "FrameCaptureHlsSettings"
      (\x -> Prelude.pure FrameCaptureHlsSettings')

instance Prelude.Hashable FrameCaptureHlsSettings

instance Prelude.NFData FrameCaptureHlsSettings

instance Prelude.ToJSON FrameCaptureHlsSettings where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)
