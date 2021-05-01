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
-- Module      : Network.AWS.EMR.Types.SupportedProductConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SupportedProductConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The list of supported product configurations that allow user-supplied
-- arguments. EMR accepts these arguments and forwards them to the
-- corresponding installation script as bootstrap action arguments.
--
-- /See:/ 'newSupportedProductConfig' smart constructor.
data SupportedProductConfig = SupportedProductConfig'
  { -- | The list of user-supplied arguments.
    args :: Prelude.Maybe [Prelude.Text],
    -- | The name of the product configuration.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SupportedProductConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'args', 'supportedProductConfig_args' - The list of user-supplied arguments.
--
-- 'name', 'supportedProductConfig_name' - The name of the product configuration.
newSupportedProductConfig ::
  SupportedProductConfig
newSupportedProductConfig =
  SupportedProductConfig'
    { args = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The list of user-supplied arguments.
supportedProductConfig_args :: Lens.Lens' SupportedProductConfig (Prelude.Maybe [Prelude.Text])
supportedProductConfig_args = Lens.lens (\SupportedProductConfig' {args} -> args) (\s@SupportedProductConfig' {} a -> s {args = a} :: SupportedProductConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the product configuration.
supportedProductConfig_name :: Lens.Lens' SupportedProductConfig (Prelude.Maybe Prelude.Text)
supportedProductConfig_name = Lens.lens (\SupportedProductConfig' {name} -> name) (\s@SupportedProductConfig' {} a -> s {name = a} :: SupportedProductConfig)

instance Prelude.Hashable SupportedProductConfig

instance Prelude.NFData SupportedProductConfig

instance Prelude.ToJSON SupportedProductConfig where
  toJSON SupportedProductConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Args" Prelude..=) Prelude.<$> args,
            ("Name" Prelude..=) Prelude.<$> name
          ]
      )
