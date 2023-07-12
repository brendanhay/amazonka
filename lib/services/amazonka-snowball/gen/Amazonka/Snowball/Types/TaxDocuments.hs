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
-- Module      : Amazonka.Snowball.Types.TaxDocuments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.TaxDocuments where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.INDTaxDocuments

-- | The tax documents required in your Amazon Web Services Region.
--
-- /See:/ 'newTaxDocuments' smart constructor.
data TaxDocuments = TaxDocuments'
  { ind :: Prelude.Maybe INDTaxDocuments
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON TaxDocuments where
  parseJSON =
    Data.withObject
      "TaxDocuments"
      (\x -> TaxDocuments' Prelude.<$> (x Data..:? "IND"))

instance Prelude.Hashable TaxDocuments where
  hashWithSalt _salt TaxDocuments' {..} =
    _salt `Prelude.hashWithSalt` ind

instance Prelude.NFData TaxDocuments where
  rnf TaxDocuments' {..} = Prelude.rnf ind

instance Data.ToJSON TaxDocuments where
  toJSON TaxDocuments' {..} =
    Data.object
      (Prelude.catMaybes [("IND" Data..=) Prelude.<$> ind])
