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
import qualified Network.AWS.Prelude as Prelude

-- |
--
-- /See:/ 'newCaptureContentTypeHeader' smart constructor.
data CaptureContentTypeHeader = CaptureContentTypeHeader'
  { csvContentTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    jsonContentTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      jsonContentTypes = Prelude.Nothing
    }

-- |
captureContentTypeHeader_csvContentTypes :: Lens.Lens' CaptureContentTypeHeader (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
captureContentTypeHeader_csvContentTypes = Lens.lens (\CaptureContentTypeHeader' {csvContentTypes} -> csvContentTypes) (\s@CaptureContentTypeHeader' {} a -> s {csvContentTypes = a} :: CaptureContentTypeHeader) Prelude.. Lens.mapping Lens._Coerce

-- |
captureContentTypeHeader_jsonContentTypes :: Lens.Lens' CaptureContentTypeHeader (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
captureContentTypeHeader_jsonContentTypes = Lens.lens (\CaptureContentTypeHeader' {jsonContentTypes} -> jsonContentTypes) (\s@CaptureContentTypeHeader' {} a -> s {jsonContentTypes = a} :: CaptureContentTypeHeader) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON CaptureContentTypeHeader where
  parseJSON =
    Core.withObject
      "CaptureContentTypeHeader"
      ( \x ->
          CaptureContentTypeHeader'
            Prelude.<$> (x Core..:? "CsvContentTypes")
            Prelude.<*> (x Core..:? "JsonContentTypes")
      )

instance Prelude.Hashable CaptureContentTypeHeader

instance Prelude.NFData CaptureContentTypeHeader

instance Core.ToJSON CaptureContentTypeHeader where
  toJSON CaptureContentTypeHeader' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CsvContentTypes" Core..=)
              Prelude.<$> csvContentTypes,
            ("JsonContentTypes" Core..=)
              Prelude.<$> jsonContentTypes
          ]
      )
