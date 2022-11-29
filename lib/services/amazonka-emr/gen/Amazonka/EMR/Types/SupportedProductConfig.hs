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
-- Module      : Amazonka.EMR.Types.SupportedProductConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.SupportedProductConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The list of supported product configurations that allow user-supplied
-- arguments. EMR accepts these arguments and forwards them to the
-- corresponding installation script as bootstrap action arguments.
--
-- /See:/ 'newSupportedProductConfig' smart constructor.
data SupportedProductConfig = SupportedProductConfig'
  { -- | The name of the product configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The list of user-supplied arguments.
    args :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SupportedProductConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'supportedProductConfig_name' - The name of the product configuration.
--
-- 'args', 'supportedProductConfig_args' - The list of user-supplied arguments.
newSupportedProductConfig ::
  SupportedProductConfig
newSupportedProductConfig =
  SupportedProductConfig'
    { name = Prelude.Nothing,
      args = Prelude.Nothing
    }

-- | The name of the product configuration.
supportedProductConfig_name :: Lens.Lens' SupportedProductConfig (Prelude.Maybe Prelude.Text)
supportedProductConfig_name = Lens.lens (\SupportedProductConfig' {name} -> name) (\s@SupportedProductConfig' {} a -> s {name = a} :: SupportedProductConfig)

-- | The list of user-supplied arguments.
supportedProductConfig_args :: Lens.Lens' SupportedProductConfig (Prelude.Maybe [Prelude.Text])
supportedProductConfig_args = Lens.lens (\SupportedProductConfig' {args} -> args) (\s@SupportedProductConfig' {} a -> s {args = a} :: SupportedProductConfig) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable SupportedProductConfig where
  hashWithSalt _salt SupportedProductConfig' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` args

instance Prelude.NFData SupportedProductConfig where
  rnf SupportedProductConfig' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf args

instance Core.ToJSON SupportedProductConfig where
  toJSON SupportedProductConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("Args" Core..=) Prelude.<$> args
          ]
      )
