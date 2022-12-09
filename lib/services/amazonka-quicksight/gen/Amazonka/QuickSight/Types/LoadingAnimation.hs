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
-- Module      : Amazonka.QuickSight.Types.LoadingAnimation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LoadingAnimation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The configuration of loading animation in free-form layout.
--
-- /See:/ 'newLoadingAnimation' smart constructor.
data LoadingAnimation = LoadingAnimation'
  { -- | The visibility configuration of @LoadingAnimation@.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadingAnimation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibility', 'loadingAnimation_visibility' - The visibility configuration of @LoadingAnimation@.
newLoadingAnimation ::
  LoadingAnimation
newLoadingAnimation =
  LoadingAnimation' {visibility = Prelude.Nothing}

-- | The visibility configuration of @LoadingAnimation@.
loadingAnimation_visibility :: Lens.Lens' LoadingAnimation (Prelude.Maybe Visibility)
loadingAnimation_visibility = Lens.lens (\LoadingAnimation' {visibility} -> visibility) (\s@LoadingAnimation' {} a -> s {visibility = a} :: LoadingAnimation)

instance Data.FromJSON LoadingAnimation where
  parseJSON =
    Data.withObject
      "LoadingAnimation"
      ( \x ->
          LoadingAnimation'
            Prelude.<$> (x Data..:? "Visibility")
      )

instance Prelude.Hashable LoadingAnimation where
  hashWithSalt _salt LoadingAnimation' {..} =
    _salt `Prelude.hashWithSalt` visibility

instance Prelude.NFData LoadingAnimation where
  rnf LoadingAnimation' {..} = Prelude.rnf visibility

instance Data.ToJSON LoadingAnimation where
  toJSON LoadingAnimation' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Visibility" Data..=) Prelude.<$> visibility]
      )
