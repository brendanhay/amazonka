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
-- Module      : Network.AWS.SageMaker.Types.CaptureOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CaptureOption where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.CaptureMode

-- |
--
-- /See:/ 'newCaptureOption' smart constructor.
data CaptureOption = CaptureOption'
  { captureMode :: CaptureMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CaptureOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captureMode', 'captureOption_captureMode' -
newCaptureOption ::
  -- | 'captureMode'
  CaptureMode ->
  CaptureOption
newCaptureOption pCaptureMode_ =
  CaptureOption' {captureMode = pCaptureMode_}

-- |
captureOption_captureMode :: Lens.Lens' CaptureOption CaptureMode
captureOption_captureMode = Lens.lens (\CaptureOption' {captureMode} -> captureMode) (\s@CaptureOption' {} a -> s {captureMode = a} :: CaptureOption)

instance Prelude.FromJSON CaptureOption where
  parseJSON =
    Prelude.withObject
      "CaptureOption"
      ( \x ->
          CaptureOption'
            Prelude.<$> (x Prelude..: "CaptureMode")
      )

instance Prelude.Hashable CaptureOption

instance Prelude.NFData CaptureOption

instance Prelude.ToJSON CaptureOption where
  toJSON CaptureOption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CaptureMode" Prelude..= captureMode)
          ]
      )
