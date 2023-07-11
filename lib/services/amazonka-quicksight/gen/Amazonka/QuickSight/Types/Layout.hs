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
-- Module      : Amazonka.QuickSight.Types.Layout
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Layout where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LayoutConfiguration

-- | A @Layout@ defines the placement of elements within a sheet.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/types-of-layout.html Types of layout>
-- in the /Amazon QuickSight User Guide/.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newLayout' smart constructor.
data Layout = Layout'
  { -- | The configuration that determines what the type of layout for a sheet.
    configuration :: LayoutConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Layout' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'layout_configuration' - The configuration that determines what the type of layout for a sheet.
newLayout ::
  -- | 'configuration'
  LayoutConfiguration ->
  Layout
newLayout pConfiguration_ =
  Layout' {configuration = pConfiguration_}

-- | The configuration that determines what the type of layout for a sheet.
layout_configuration :: Lens.Lens' Layout LayoutConfiguration
layout_configuration = Lens.lens (\Layout' {configuration} -> configuration) (\s@Layout' {} a -> s {configuration = a} :: Layout)

instance Data.FromJSON Layout where
  parseJSON =
    Data.withObject
      "Layout"
      ( \x ->
          Layout' Prelude.<$> (x Data..: "Configuration")
      )

instance Prelude.Hashable Layout where
  hashWithSalt _salt Layout' {..} =
    _salt `Prelude.hashWithSalt` configuration

instance Prelude.NFData Layout where
  rnf Layout' {..} = Prelude.rnf configuration

instance Data.ToJSON Layout where
  toJSON Layout' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("Configuration" Data..= configuration)
          ]
      )
