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
-- Module      : Amazonka.QuickSight.Types.TextControlPlaceholderOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TextControlPlaceholderOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The configuration of the placeholder options in a text control.
--
-- /See:/ 'newTextControlPlaceholderOptions' smart constructor.
data TextControlPlaceholderOptions = TextControlPlaceholderOptions'
  { -- | The visibility configuration of the placeholder options in a text
    -- control.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextControlPlaceholderOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibility', 'textControlPlaceholderOptions_visibility' - The visibility configuration of the placeholder options in a text
-- control.
newTextControlPlaceholderOptions ::
  TextControlPlaceholderOptions
newTextControlPlaceholderOptions =
  TextControlPlaceholderOptions'
    { visibility =
        Prelude.Nothing
    }

-- | The visibility configuration of the placeholder options in a text
-- control.
textControlPlaceholderOptions_visibility :: Lens.Lens' TextControlPlaceholderOptions (Prelude.Maybe Visibility)
textControlPlaceholderOptions_visibility = Lens.lens (\TextControlPlaceholderOptions' {visibility} -> visibility) (\s@TextControlPlaceholderOptions' {} a -> s {visibility = a} :: TextControlPlaceholderOptions)

instance Data.FromJSON TextControlPlaceholderOptions where
  parseJSON =
    Data.withObject
      "TextControlPlaceholderOptions"
      ( \x ->
          TextControlPlaceholderOptions'
            Prelude.<$> (x Data..:? "Visibility")
      )

instance
  Prelude.Hashable
    TextControlPlaceholderOptions
  where
  hashWithSalt _salt TextControlPlaceholderOptions' {..} =
    _salt `Prelude.hashWithSalt` visibility

instance Prelude.NFData TextControlPlaceholderOptions where
  rnf TextControlPlaceholderOptions' {..} =
    Prelude.rnf visibility

instance Data.ToJSON TextControlPlaceholderOptions where
  toJSON TextControlPlaceholderOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Visibility" Data..=) Prelude.<$> visibility]
      )
