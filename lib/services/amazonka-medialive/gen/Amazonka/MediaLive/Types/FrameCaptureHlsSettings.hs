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
-- Module      : Amazonka.MediaLive.Types.FrameCaptureHlsSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FrameCaptureHlsSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Frame Capture Hls Settings
--
-- /See:/ 'newFrameCaptureHlsSettings' smart constructor.
data FrameCaptureHlsSettings = FrameCaptureHlsSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrameCaptureHlsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newFrameCaptureHlsSettings ::
  FrameCaptureHlsSettings
newFrameCaptureHlsSettings = FrameCaptureHlsSettings'

instance Data.FromJSON FrameCaptureHlsSettings where
  parseJSON =
    Data.withObject
      "FrameCaptureHlsSettings"
      (\x -> Prelude.pure FrameCaptureHlsSettings')

instance Prelude.Hashable FrameCaptureHlsSettings where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData FrameCaptureHlsSettings where
  rnf _ = ()

instance Data.ToJSON FrameCaptureHlsSettings where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
