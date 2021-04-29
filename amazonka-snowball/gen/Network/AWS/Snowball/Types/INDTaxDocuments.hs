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
-- Module      : Network.AWS.Snowball.Types.INDTaxDocuments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.INDTaxDocuments where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The tax documents required in AWS Regions in India.
--
-- /See:/ 'newINDTaxDocuments' smart constructor.
data INDTaxDocuments = INDTaxDocuments'
  { -- | The Goods and Services Tax (GST) documents required in AWS Regions in
    -- India.
    gstin :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'INDTaxDocuments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gstin', 'iNDTaxDocuments_gstin' - The Goods and Services Tax (GST) documents required in AWS Regions in
-- India.
newINDTaxDocuments ::
  INDTaxDocuments
newINDTaxDocuments =
  INDTaxDocuments' {gstin = Prelude.Nothing}

-- | The Goods and Services Tax (GST) documents required in AWS Regions in
-- India.
iNDTaxDocuments_gstin :: Lens.Lens' INDTaxDocuments (Prelude.Maybe Prelude.Text)
iNDTaxDocuments_gstin = Lens.lens (\INDTaxDocuments' {gstin} -> gstin) (\s@INDTaxDocuments' {} a -> s {gstin = a} :: INDTaxDocuments)

instance Prelude.FromJSON INDTaxDocuments where
  parseJSON =
    Prelude.withObject
      "INDTaxDocuments"
      ( \x ->
          INDTaxDocuments' Prelude.<$> (x Prelude..:? "GSTIN")
      )

instance Prelude.Hashable INDTaxDocuments

instance Prelude.NFData INDTaxDocuments

instance Prelude.ToJSON INDTaxDocuments where
  toJSON INDTaxDocuments' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("GSTIN" Prelude..=) Prelude.<$> gstin]
      )
