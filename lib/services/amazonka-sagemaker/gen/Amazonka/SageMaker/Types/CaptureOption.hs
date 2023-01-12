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
-- Module      : Amazonka.SageMaker.Types.CaptureOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CaptureOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CaptureMode

-- | Specifies data Model Monitor will capture.
--
-- /See:/ 'newCaptureOption' smart constructor.
data CaptureOption = CaptureOption'
  { -- | Specify the boundary of data to capture.
    captureMode :: CaptureMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptureOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captureMode', 'captureOption_captureMode' - Specify the boundary of data to capture.
newCaptureOption ::
  -- | 'captureMode'
  CaptureMode ->
  CaptureOption
newCaptureOption pCaptureMode_ =
  CaptureOption' {captureMode = pCaptureMode_}

-- | Specify the boundary of data to capture.
captureOption_captureMode :: Lens.Lens' CaptureOption CaptureMode
captureOption_captureMode = Lens.lens (\CaptureOption' {captureMode} -> captureMode) (\s@CaptureOption' {} a -> s {captureMode = a} :: CaptureOption)

instance Data.FromJSON CaptureOption where
  parseJSON =
    Data.withObject
      "CaptureOption"
      ( \x ->
          CaptureOption' Prelude.<$> (x Data..: "CaptureMode")
      )

instance Prelude.Hashable CaptureOption where
  hashWithSalt _salt CaptureOption' {..} =
    _salt `Prelude.hashWithSalt` captureMode

instance Prelude.NFData CaptureOption where
  rnf CaptureOption' {..} = Prelude.rnf captureMode

instance Data.ToJSON CaptureOption where
  toJSON CaptureOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("CaptureMode" Data..= captureMode)]
      )
