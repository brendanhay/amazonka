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
-- Module      : Amazonka.SecurityHub.Types.StandardsManagedBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StandardsManagedBy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the management of a security standard.
--
-- /See:/ 'newStandardsManagedBy' smart constructor.
data StandardsManagedBy = StandardsManagedBy'
  { -- | An identifier for the company that manages a specific security standard.
    -- For existing standards, the value is equal to @Amazon Web Services@.
    company :: Prelude.Maybe Prelude.Text,
    -- | An identifier for the product that manages a specific security standard.
    -- For existing standards, the value is equal to the Amazon Web Services
    -- service that manages the standard.
    product :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StandardsManagedBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'company', 'standardsManagedBy_company' - An identifier for the company that manages a specific security standard.
-- For existing standards, the value is equal to @Amazon Web Services@.
--
-- 'product', 'standardsManagedBy_product' - An identifier for the product that manages a specific security standard.
-- For existing standards, the value is equal to the Amazon Web Services
-- service that manages the standard.
newStandardsManagedBy ::
  StandardsManagedBy
newStandardsManagedBy =
  StandardsManagedBy'
    { company = Prelude.Nothing,
      product = Prelude.Nothing
    }

-- | An identifier for the company that manages a specific security standard.
-- For existing standards, the value is equal to @Amazon Web Services@.
standardsManagedBy_company :: Lens.Lens' StandardsManagedBy (Prelude.Maybe Prelude.Text)
standardsManagedBy_company = Lens.lens (\StandardsManagedBy' {company} -> company) (\s@StandardsManagedBy' {} a -> s {company = a} :: StandardsManagedBy)

-- | An identifier for the product that manages a specific security standard.
-- For existing standards, the value is equal to the Amazon Web Services
-- service that manages the standard.
standardsManagedBy_product :: Lens.Lens' StandardsManagedBy (Prelude.Maybe Prelude.Text)
standardsManagedBy_product = Lens.lens (\StandardsManagedBy' {product} -> product) (\s@StandardsManagedBy' {} a -> s {product = a} :: StandardsManagedBy)

instance Data.FromJSON StandardsManagedBy where
  parseJSON =
    Data.withObject
      "StandardsManagedBy"
      ( \x ->
          StandardsManagedBy'
            Prelude.<$> (x Data..:? "Company")
            Prelude.<*> (x Data..:? "Product")
      )

instance Prelude.Hashable StandardsManagedBy where
  hashWithSalt _salt StandardsManagedBy' {..} =
    _salt `Prelude.hashWithSalt` company
      `Prelude.hashWithSalt` product

instance Prelude.NFData StandardsManagedBy where
  rnf StandardsManagedBy' {..} =
    Prelude.rnf company
      `Prelude.seq` Prelude.rnf product
