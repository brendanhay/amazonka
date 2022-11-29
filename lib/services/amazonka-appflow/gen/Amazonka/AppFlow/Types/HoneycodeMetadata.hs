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
-- Module      : Amazonka.AppFlow.Types.HoneycodeMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.HoneycodeMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector metadata specific to Amazon Honeycode.
--
-- /See:/ 'newHoneycodeMetadata' smart constructor.
data HoneycodeMetadata = HoneycodeMetadata'
  { -- | The desired authorization scope for the Amazon Honeycode account.
    oAuthScopes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HoneycodeMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oAuthScopes', 'honeycodeMetadata_oAuthScopes' - The desired authorization scope for the Amazon Honeycode account.
newHoneycodeMetadata ::
  HoneycodeMetadata
newHoneycodeMetadata =
  HoneycodeMetadata' {oAuthScopes = Prelude.Nothing}

-- | The desired authorization scope for the Amazon Honeycode account.
honeycodeMetadata_oAuthScopes :: Lens.Lens' HoneycodeMetadata (Prelude.Maybe [Prelude.Text])
honeycodeMetadata_oAuthScopes = Lens.lens (\HoneycodeMetadata' {oAuthScopes} -> oAuthScopes) (\s@HoneycodeMetadata' {} a -> s {oAuthScopes = a} :: HoneycodeMetadata) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON HoneycodeMetadata where
  parseJSON =
    Core.withObject
      "HoneycodeMetadata"
      ( \x ->
          HoneycodeMetadata'
            Prelude.<$> (x Core..:? "oAuthScopes" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable HoneycodeMetadata where
  hashWithSalt _salt HoneycodeMetadata' {..} =
    _salt `Prelude.hashWithSalt` oAuthScopes

instance Prelude.NFData HoneycodeMetadata where
  rnf HoneycodeMetadata' {..} = Prelude.rnf oAuthScopes
