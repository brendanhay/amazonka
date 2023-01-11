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
-- Module      : Amazonka.QuickSight.Types.ReferenceLineCustomLabelConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ReferenceLineCustomLabelConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a custom label on a @ReferenceLine@.
--
-- /See:/ 'newReferenceLineCustomLabelConfiguration' smart constructor.
data ReferenceLineCustomLabelConfiguration = ReferenceLineCustomLabelConfiguration'
  { -- | The string text of the custom label.
    customLabel :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceLineCustomLabelConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customLabel', 'referenceLineCustomLabelConfiguration_customLabel' - The string text of the custom label.
newReferenceLineCustomLabelConfiguration ::
  -- | 'customLabel'
  Prelude.Text ->
  ReferenceLineCustomLabelConfiguration
newReferenceLineCustomLabelConfiguration
  pCustomLabel_ =
    ReferenceLineCustomLabelConfiguration'
      { customLabel =
          pCustomLabel_
      }

-- | The string text of the custom label.
referenceLineCustomLabelConfiguration_customLabel :: Lens.Lens' ReferenceLineCustomLabelConfiguration Prelude.Text
referenceLineCustomLabelConfiguration_customLabel = Lens.lens (\ReferenceLineCustomLabelConfiguration' {customLabel} -> customLabel) (\s@ReferenceLineCustomLabelConfiguration' {} a -> s {customLabel = a} :: ReferenceLineCustomLabelConfiguration)

instance
  Data.FromJSON
    ReferenceLineCustomLabelConfiguration
  where
  parseJSON =
    Data.withObject
      "ReferenceLineCustomLabelConfiguration"
      ( \x ->
          ReferenceLineCustomLabelConfiguration'
            Prelude.<$> (x Data..: "CustomLabel")
      )

instance
  Prelude.Hashable
    ReferenceLineCustomLabelConfiguration
  where
  hashWithSalt
    _salt
    ReferenceLineCustomLabelConfiguration' {..} =
      _salt `Prelude.hashWithSalt` customLabel

instance
  Prelude.NFData
    ReferenceLineCustomLabelConfiguration
  where
  rnf ReferenceLineCustomLabelConfiguration' {..} =
    Prelude.rnf customLabel

instance
  Data.ToJSON
    ReferenceLineCustomLabelConfiguration
  where
  toJSON ReferenceLineCustomLabelConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("CustomLabel" Data..= customLabel)]
      )
