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
-- Module      : Network.AWS.Snowball.Types.TaxDocuments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.TaxDocuments where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Snowball.Types.INDTaxDocuments

-- | The tax documents required in your AWS Region.
--
-- /See:/ 'newTaxDocuments' smart constructor.
data TaxDocuments = TaxDocuments'
  { ind :: Prelude.Maybe INDTaxDocuments
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TaxDocuments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ind', 'taxDocuments_ind' - Undocumented member.
newTaxDocuments ::
  TaxDocuments
newTaxDocuments =
  TaxDocuments' {ind = Prelude.Nothing}

-- | Undocumented member.
taxDocuments_ind :: Lens.Lens' TaxDocuments (Prelude.Maybe INDTaxDocuments)
taxDocuments_ind = Lens.lens (\TaxDocuments' {ind} -> ind) (\s@TaxDocuments' {} a -> s {ind = a} :: TaxDocuments)

instance Prelude.FromJSON TaxDocuments where
  parseJSON =
    Prelude.withObject
      "TaxDocuments"
      ( \x ->
          TaxDocuments' Prelude.<$> (x Prelude..:? "IND")
      )

instance Prelude.Hashable TaxDocuments

instance Prelude.NFData TaxDocuments

instance Prelude.ToJSON TaxDocuments where
  toJSON TaxDocuments' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("IND" Prelude..=) Prelude.<$> ind]
      )
