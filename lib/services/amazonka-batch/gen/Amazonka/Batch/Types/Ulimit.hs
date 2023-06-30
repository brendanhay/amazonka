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
-- Module      : Amazonka.Batch.Types.Ulimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.Ulimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The @ulimit@ settings to pass to the container.
--
-- This object isn\'t applicable to jobs that are running on Fargate
-- resources.
--
-- /See:/ 'newUlimit' smart constructor.
data Ulimit = Ulimit'
  { -- | The hard limit for the @ulimit@ type.
    hardLimit :: Prelude.Int,
    -- | The @type@ of the @ulimit@.
    name :: Prelude.Text,
    -- | The soft limit for the @ulimit@ type.
    softLimit :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ulimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hardLimit', 'ulimit_hardLimit' - The hard limit for the @ulimit@ type.
--
-- 'name', 'ulimit_name' - The @type@ of the @ulimit@.
--
-- 'softLimit', 'ulimit_softLimit' - The soft limit for the @ulimit@ type.
newUlimit ::
  -- | 'hardLimit'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'softLimit'
  Prelude.Int ->
  Ulimit
newUlimit pHardLimit_ pName_ pSoftLimit_ =
  Ulimit'
    { hardLimit = pHardLimit_,
      name = pName_,
      softLimit = pSoftLimit_
    }

-- | The hard limit for the @ulimit@ type.
ulimit_hardLimit :: Lens.Lens' Ulimit Prelude.Int
ulimit_hardLimit = Lens.lens (\Ulimit' {hardLimit} -> hardLimit) (\s@Ulimit' {} a -> s {hardLimit = a} :: Ulimit)

-- | The @type@ of the @ulimit@.
ulimit_name :: Lens.Lens' Ulimit Prelude.Text
ulimit_name = Lens.lens (\Ulimit' {name} -> name) (\s@Ulimit' {} a -> s {name = a} :: Ulimit)

-- | The soft limit for the @ulimit@ type.
ulimit_softLimit :: Lens.Lens' Ulimit Prelude.Int
ulimit_softLimit = Lens.lens (\Ulimit' {softLimit} -> softLimit) (\s@Ulimit' {} a -> s {softLimit = a} :: Ulimit)

instance Data.FromJSON Ulimit where
  parseJSON =
    Data.withObject
      "Ulimit"
      ( \x ->
          Ulimit'
            Prelude.<$> (x Data..: "hardLimit")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "softLimit")
      )

instance Prelude.Hashable Ulimit where
  hashWithSalt _salt Ulimit' {..} =
    _salt
      `Prelude.hashWithSalt` hardLimit
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` softLimit

instance Prelude.NFData Ulimit where
  rnf Ulimit' {..} =
    Prelude.rnf hardLimit
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf softLimit

instance Data.ToJSON Ulimit where
  toJSON Ulimit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("hardLimit" Data..= hardLimit),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("softLimit" Data..= softLimit)
          ]
      )
