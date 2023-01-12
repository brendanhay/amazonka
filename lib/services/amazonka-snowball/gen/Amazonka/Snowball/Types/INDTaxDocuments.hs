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
-- Module      : Amazonka.Snowball.Types.INDTaxDocuments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.INDTaxDocuments where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The tax documents required in Amazon Web Services Region in India.
--
-- /See:/ 'newINDTaxDocuments' smart constructor.
data INDTaxDocuments = INDTaxDocuments'
  { -- | The Goods and Services Tax (GST) documents required in Amazon Web
    -- Services Region in India.
    gstin :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'INDTaxDocuments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gstin', 'iNDTaxDocuments_gstin' - The Goods and Services Tax (GST) documents required in Amazon Web
-- Services Region in India.
newINDTaxDocuments ::
  INDTaxDocuments
newINDTaxDocuments =
  INDTaxDocuments' {gstin = Prelude.Nothing}

-- | The Goods and Services Tax (GST) documents required in Amazon Web
-- Services Region in India.
iNDTaxDocuments_gstin :: Lens.Lens' INDTaxDocuments (Prelude.Maybe Prelude.Text)
iNDTaxDocuments_gstin = Lens.lens (\INDTaxDocuments' {gstin} -> gstin) (\s@INDTaxDocuments' {} a -> s {gstin = a} :: INDTaxDocuments)

instance Data.FromJSON INDTaxDocuments where
  parseJSON =
    Data.withObject
      "INDTaxDocuments"
      ( \x ->
          INDTaxDocuments' Prelude.<$> (x Data..:? "GSTIN")
      )

instance Prelude.Hashable INDTaxDocuments where
  hashWithSalt _salt INDTaxDocuments' {..} =
    _salt `Prelude.hashWithSalt` gstin

instance Prelude.NFData INDTaxDocuments where
  rnf INDTaxDocuments' {..} = Prelude.rnf gstin

instance Data.ToJSON INDTaxDocuments where
  toJSON INDTaxDocuments' {..} =
    Data.object
      ( Prelude.catMaybes
          [("GSTIN" Data..=) Prelude.<$> gstin]
      )
