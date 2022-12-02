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
-- Module      : Amazonka.ConnectCases.Types.LayoutConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.LayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object to store configuration of layouts associated to the template.
--
-- /See:/ 'newLayoutConfiguration' smart constructor.
data LayoutConfiguration = LayoutConfiguration'
  { -- | Unique identifier of a layout.
    defaultLayout :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultLayout', 'layoutConfiguration_defaultLayout' - Unique identifier of a layout.
newLayoutConfiguration ::
  LayoutConfiguration
newLayoutConfiguration =
  LayoutConfiguration'
    { defaultLayout =
        Prelude.Nothing
    }

-- | Unique identifier of a layout.
layoutConfiguration_defaultLayout :: Lens.Lens' LayoutConfiguration (Prelude.Maybe Prelude.Text)
layoutConfiguration_defaultLayout = Lens.lens (\LayoutConfiguration' {defaultLayout} -> defaultLayout) (\s@LayoutConfiguration' {} a -> s {defaultLayout = a} :: LayoutConfiguration)

instance Data.FromJSON LayoutConfiguration where
  parseJSON =
    Data.withObject
      "LayoutConfiguration"
      ( \x ->
          LayoutConfiguration'
            Prelude.<$> (x Data..:? "defaultLayout")
      )

instance Prelude.Hashable LayoutConfiguration where
  hashWithSalt _salt LayoutConfiguration' {..} =
    _salt `Prelude.hashWithSalt` defaultLayout

instance Prelude.NFData LayoutConfiguration where
  rnf LayoutConfiguration' {..} =
    Prelude.rnf defaultLayout

instance Data.ToJSON LayoutConfiguration where
  toJSON LayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("defaultLayout" Data..=)
              Prelude.<$> defaultLayout
          ]
      )
