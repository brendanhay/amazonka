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
-- Module      : Amazonka.QuickSight.Types.CascadingControlConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CascadingControlConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CascadingControlSource

-- | The values that are displayed in a control can be configured to only
-- show values that are valid based on what\'s selected in other controls.
--
-- /See:/ 'newCascadingControlConfiguration' smart constructor.
data CascadingControlConfiguration = CascadingControlConfiguration'
  { -- | A list of source controls that determine the values that are used in the
    -- current control.
    sourceControls :: Prelude.Maybe [CascadingControlSource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CascadingControlConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceControls', 'cascadingControlConfiguration_sourceControls' - A list of source controls that determine the values that are used in the
-- current control.
newCascadingControlConfiguration ::
  CascadingControlConfiguration
newCascadingControlConfiguration =
  CascadingControlConfiguration'
    { sourceControls =
        Prelude.Nothing
    }

-- | A list of source controls that determine the values that are used in the
-- current control.
cascadingControlConfiguration_sourceControls :: Lens.Lens' CascadingControlConfiguration (Prelude.Maybe [CascadingControlSource])
cascadingControlConfiguration_sourceControls = Lens.lens (\CascadingControlConfiguration' {sourceControls} -> sourceControls) (\s@CascadingControlConfiguration' {} a -> s {sourceControls = a} :: CascadingControlConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CascadingControlConfiguration where
  parseJSON =
    Data.withObject
      "CascadingControlConfiguration"
      ( \x ->
          CascadingControlConfiguration'
            Prelude.<$> ( x
                            Data..:? "SourceControls"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    CascadingControlConfiguration
  where
  hashWithSalt _salt CascadingControlConfiguration' {..} =
    _salt `Prelude.hashWithSalt` sourceControls

instance Prelude.NFData CascadingControlConfiguration where
  rnf CascadingControlConfiguration' {..} =
    Prelude.rnf sourceControls

instance Data.ToJSON CascadingControlConfiguration where
  toJSON CascadingControlConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SourceControls" Data..=)
              Prelude.<$> sourceControls
          ]
      )
