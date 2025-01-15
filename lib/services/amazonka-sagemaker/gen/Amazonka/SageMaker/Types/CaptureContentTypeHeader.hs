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
-- Module      : Amazonka.SageMaker.Types.CaptureContentTypeHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CaptureContentTypeHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration specifying how to treat different headers. If no headers
-- are specified SageMaker will by default base64 encode when capturing the
-- data.
--
-- /See:/ 'newCaptureContentTypeHeader' smart constructor.
data CaptureContentTypeHeader = CaptureContentTypeHeader'
  { -- | The list of all content type headers that SageMaker will treat as CSV
    -- and capture accordingly.
    csvContentTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The list of all content type headers that SageMaker will treat as JSON
    -- and capture accordingly.
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
-- 'csvContentTypes', 'captureContentTypeHeader_csvContentTypes' - The list of all content type headers that SageMaker will treat as CSV
-- and capture accordingly.
--
-- 'jsonContentTypes', 'captureContentTypeHeader_jsonContentTypes' - The list of all content type headers that SageMaker will treat as JSON
-- and capture accordingly.
newCaptureContentTypeHeader ::
  CaptureContentTypeHeader
newCaptureContentTypeHeader =
  CaptureContentTypeHeader'
    { csvContentTypes =
        Prelude.Nothing,
      jsonContentTypes = Prelude.Nothing
    }

-- | The list of all content type headers that SageMaker will treat as CSV
-- and capture accordingly.
captureContentTypeHeader_csvContentTypes :: Lens.Lens' CaptureContentTypeHeader (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
captureContentTypeHeader_csvContentTypes = Lens.lens (\CaptureContentTypeHeader' {csvContentTypes} -> csvContentTypes) (\s@CaptureContentTypeHeader' {} a -> s {csvContentTypes = a} :: CaptureContentTypeHeader) Prelude.. Lens.mapping Lens.coerced

-- | The list of all content type headers that SageMaker will treat as JSON
-- and capture accordingly.
captureContentTypeHeader_jsonContentTypes :: Lens.Lens' CaptureContentTypeHeader (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
captureContentTypeHeader_jsonContentTypes = Lens.lens (\CaptureContentTypeHeader' {jsonContentTypes} -> jsonContentTypes) (\s@CaptureContentTypeHeader' {} a -> s {jsonContentTypes = a} :: CaptureContentTypeHeader) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CaptureContentTypeHeader where
  parseJSON =
    Data.withObject
      "CaptureContentTypeHeader"
      ( \x ->
          CaptureContentTypeHeader'
            Prelude.<$> (x Data..:? "CsvContentTypes")
            Prelude.<*> (x Data..:? "JsonContentTypes")
      )

instance Prelude.Hashable CaptureContentTypeHeader where
  hashWithSalt _salt CaptureContentTypeHeader' {..} =
    _salt
      `Prelude.hashWithSalt` csvContentTypes
      `Prelude.hashWithSalt` jsonContentTypes

instance Prelude.NFData CaptureContentTypeHeader where
  rnf CaptureContentTypeHeader' {..} =
    Prelude.rnf csvContentTypes `Prelude.seq`
      Prelude.rnf jsonContentTypes

instance Data.ToJSON CaptureContentTypeHeader where
  toJSON CaptureContentTypeHeader' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CsvContentTypes" Data..=)
              Prelude.<$> csvContentTypes,
            ("JsonContentTypes" Data..=)
              Prelude.<$> jsonContentTypes
          ]
      )
