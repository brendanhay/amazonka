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
-- Module      : Amazonka.Support.Types.SupportService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.SupportService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Support.Types.Category

-- | Information about an Amazon Web Services service returned by the
-- DescribeServices operation.
--
-- /See:/ 'newSupportService' smart constructor.
data SupportService = SupportService'
  { -- | A list of categories that describe the type of support issue a case
    -- describes. Categories consist of a category name and a category code.
    -- Category names and codes are passed to Amazon Web Services Support when
    -- you call CreateCase.
    categories :: Prelude.Maybe [Category],
    -- | The code for an Amazon Web Services service returned by the
    -- DescribeServices response. The @name@ element contains the corresponding
    -- friendly name.
    code :: Prelude.Maybe Prelude.Text,
    -- | The friendly name for an Amazon Web Services service. The @code@ element
    -- contains the corresponding code.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SupportService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categories', 'supportService_categories' - A list of categories that describe the type of support issue a case
-- describes. Categories consist of a category name and a category code.
-- Category names and codes are passed to Amazon Web Services Support when
-- you call CreateCase.
--
-- 'code', 'supportService_code' - The code for an Amazon Web Services service returned by the
-- DescribeServices response. The @name@ element contains the corresponding
-- friendly name.
--
-- 'name', 'supportService_name' - The friendly name for an Amazon Web Services service. The @code@ element
-- contains the corresponding code.
newSupportService ::
  SupportService
newSupportService =
  SupportService'
    { categories = Prelude.Nothing,
      code = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | A list of categories that describe the type of support issue a case
-- describes. Categories consist of a category name and a category code.
-- Category names and codes are passed to Amazon Web Services Support when
-- you call CreateCase.
supportService_categories :: Lens.Lens' SupportService (Prelude.Maybe [Category])
supportService_categories = Lens.lens (\SupportService' {categories} -> categories) (\s@SupportService' {} a -> s {categories = a} :: SupportService) Prelude.. Lens.mapping Lens.coerced

-- | The code for an Amazon Web Services service returned by the
-- DescribeServices response. The @name@ element contains the corresponding
-- friendly name.
supportService_code :: Lens.Lens' SupportService (Prelude.Maybe Prelude.Text)
supportService_code = Lens.lens (\SupportService' {code} -> code) (\s@SupportService' {} a -> s {code = a} :: SupportService)

-- | The friendly name for an Amazon Web Services service. The @code@ element
-- contains the corresponding code.
supportService_name :: Lens.Lens' SupportService (Prelude.Maybe Prelude.Text)
supportService_name = Lens.lens (\SupportService' {name} -> name) (\s@SupportService' {} a -> s {name = a} :: SupportService)

instance Data.FromJSON SupportService where
  parseJSON =
    Data.withObject
      "SupportService"
      ( \x ->
          SupportService'
            Prelude.<$> (x Data..:? "categories" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "code")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable SupportService where
  hashWithSalt _salt SupportService' {..} =
    _salt
      `Prelude.hashWithSalt` categories
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` name

instance Prelude.NFData SupportService where
  rnf SupportService' {..} =
    Prelude.rnf categories `Prelude.seq`
      Prelude.rnf code `Prelude.seq`
        Prelude.rnf name
