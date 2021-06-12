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
-- Module      : Network.AWS.SageMaker.Types.CaptureContentTypeHeader
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CaptureContentTypeHeader where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- |
--
-- /See:/ 'newCaptureContentTypeHeader' smart constructor.
data CaptureContentTypeHeader = CaptureContentTypeHeader'
  { csvContentTypes :: Core.Maybe (Core.NonEmpty Core.Text),
    jsonContentTypes :: Core.Maybe (Core.NonEmpty Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CaptureContentTypeHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csvContentTypes', 'captureContentTypeHeader_csvContentTypes' -
--
-- 'jsonContentTypes', 'captureContentTypeHeader_jsonContentTypes' -
newCaptureContentTypeHeader ::
  CaptureContentTypeHeader
newCaptureContentTypeHeader =
  CaptureContentTypeHeader'
    { csvContentTypes =
        Core.Nothing,
      jsonContentTypes = Core.Nothing
    }

-- |
captureContentTypeHeader_csvContentTypes :: Lens.Lens' CaptureContentTypeHeader (Core.Maybe (Core.NonEmpty Core.Text))
captureContentTypeHeader_csvContentTypes = Lens.lens (\CaptureContentTypeHeader' {csvContentTypes} -> csvContentTypes) (\s@CaptureContentTypeHeader' {} a -> s {csvContentTypes = a} :: CaptureContentTypeHeader) Core.. Lens.mapping Lens._Coerce

-- |
captureContentTypeHeader_jsonContentTypes :: Lens.Lens' CaptureContentTypeHeader (Core.Maybe (Core.NonEmpty Core.Text))
captureContentTypeHeader_jsonContentTypes = Lens.lens (\CaptureContentTypeHeader' {jsonContentTypes} -> jsonContentTypes) (\s@CaptureContentTypeHeader' {} a -> s {jsonContentTypes = a} :: CaptureContentTypeHeader) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON CaptureContentTypeHeader where
  parseJSON =
    Core.withObject
      "CaptureContentTypeHeader"
      ( \x ->
          CaptureContentTypeHeader'
            Core.<$> (x Core..:? "CsvContentTypes")
            Core.<*> (x Core..:? "JsonContentTypes")
      )

instance Core.Hashable CaptureContentTypeHeader

instance Core.NFData CaptureContentTypeHeader

instance Core.ToJSON CaptureContentTypeHeader where
  toJSON CaptureContentTypeHeader' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CsvContentTypes" Core..=)
              Core.<$> csvContentTypes,
            ("JsonContentTypes" Core..=)
              Core.<$> jsonContentTypes
          ]
      )
