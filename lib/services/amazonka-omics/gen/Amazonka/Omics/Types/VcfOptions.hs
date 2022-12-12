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
-- Module      : Amazonka.Omics.Types.VcfOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.VcfOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Formatting options for a VCF file.
--
-- /See:/ 'newVcfOptions' smart constructor.
data VcfOptions = VcfOptions'
  { -- | The file\'s ignore filter field setting.
    ignoreFilterField :: Prelude.Maybe Prelude.Bool,
    -- | The file\'s ignore qual field setting.
    ignoreQualField :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VcfOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ignoreFilterField', 'vcfOptions_ignoreFilterField' - The file\'s ignore filter field setting.
--
-- 'ignoreQualField', 'vcfOptions_ignoreQualField' - The file\'s ignore qual field setting.
newVcfOptions ::
  VcfOptions
newVcfOptions =
  VcfOptions'
    { ignoreFilterField = Prelude.Nothing,
      ignoreQualField = Prelude.Nothing
    }

-- | The file\'s ignore filter field setting.
vcfOptions_ignoreFilterField :: Lens.Lens' VcfOptions (Prelude.Maybe Prelude.Bool)
vcfOptions_ignoreFilterField = Lens.lens (\VcfOptions' {ignoreFilterField} -> ignoreFilterField) (\s@VcfOptions' {} a -> s {ignoreFilterField = a} :: VcfOptions)

-- | The file\'s ignore qual field setting.
vcfOptions_ignoreQualField :: Lens.Lens' VcfOptions (Prelude.Maybe Prelude.Bool)
vcfOptions_ignoreQualField = Lens.lens (\VcfOptions' {ignoreQualField} -> ignoreQualField) (\s@VcfOptions' {} a -> s {ignoreQualField = a} :: VcfOptions)

instance Data.FromJSON VcfOptions where
  parseJSON =
    Data.withObject
      "VcfOptions"
      ( \x ->
          VcfOptions'
            Prelude.<$> (x Data..:? "ignoreFilterField")
            Prelude.<*> (x Data..:? "ignoreQualField")
      )

instance Prelude.Hashable VcfOptions where
  hashWithSalt _salt VcfOptions' {..} =
    _salt `Prelude.hashWithSalt` ignoreFilterField
      `Prelude.hashWithSalt` ignoreQualField

instance Prelude.NFData VcfOptions where
  rnf VcfOptions' {..} =
    Prelude.rnf ignoreFilterField
      `Prelude.seq` Prelude.rnf ignoreQualField

instance Data.ToJSON VcfOptions where
  toJSON VcfOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ignoreFilterField" Data..=)
              Prelude.<$> ignoreFilterField,
            ("ignoreQualField" Data..=)
              Prelude.<$> ignoreQualField
          ]
      )
