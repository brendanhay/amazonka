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
-- Module      : Network.AWS.Batch.Types.Ulimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Ulimit where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The @ulimit@ settings to pass to the container.
--
-- This object isn\'t applicable to jobs running on Fargate resources.
--
-- /See:/ 'newUlimit' smart constructor.
data Ulimit = Ulimit'
  { -- | The hard limit for the @ulimit@ type.
    hardLimit :: Core.Int,
    -- | The @type@ of the @ulimit@.
    name :: Core.Text,
    -- | The soft limit for the @ulimit@ type.
    softLimit :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'name'
  Core.Text ->
  -- | 'softLimit'
  Core.Int ->
  Ulimit
newUlimit pHardLimit_ pName_ pSoftLimit_ =
  Ulimit'
    { hardLimit = pHardLimit_,
      name = pName_,
      softLimit = pSoftLimit_
    }

-- | The hard limit for the @ulimit@ type.
ulimit_hardLimit :: Lens.Lens' Ulimit Core.Int
ulimit_hardLimit = Lens.lens (\Ulimit' {hardLimit} -> hardLimit) (\s@Ulimit' {} a -> s {hardLimit = a} :: Ulimit)

-- | The @type@ of the @ulimit@.
ulimit_name :: Lens.Lens' Ulimit Core.Text
ulimit_name = Lens.lens (\Ulimit' {name} -> name) (\s@Ulimit' {} a -> s {name = a} :: Ulimit)

-- | The soft limit for the @ulimit@ type.
ulimit_softLimit :: Lens.Lens' Ulimit Core.Int
ulimit_softLimit = Lens.lens (\Ulimit' {softLimit} -> softLimit) (\s@Ulimit' {} a -> s {softLimit = a} :: Ulimit)

instance Core.FromJSON Ulimit where
  parseJSON =
    Core.withObject
      "Ulimit"
      ( \x ->
          Ulimit'
            Core.<$> (x Core..: "hardLimit")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "softLimit")
      )

instance Core.Hashable Ulimit

instance Core.NFData Ulimit

instance Core.ToJSON Ulimit where
  toJSON Ulimit' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("hardLimit" Core..= hardLimit),
            Core.Just ("name" Core..= name),
            Core.Just ("softLimit" Core..= softLimit)
          ]
      )
