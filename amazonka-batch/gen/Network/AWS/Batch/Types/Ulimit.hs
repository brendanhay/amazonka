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
-- Module      : Network.AWS.Batch.Types.Ulimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Ulimit where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The @ulimit@ settings to pass to the container.
--
-- This object isn\'t applicable to jobs running on Fargate resources.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON Ulimit where
  parseJSON =
    Prelude.withObject
      "Ulimit"
      ( \x ->
          Ulimit'
            Prelude.<$> (x Prelude..: "hardLimit")
            Prelude.<*> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "softLimit")
      )

instance Prelude.Hashable Ulimit

instance Prelude.NFData Ulimit

instance Prelude.ToJSON Ulimit where
  toJSON Ulimit' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("hardLimit" Prelude..= hardLimit),
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("softLimit" Prelude..= softLimit)
          ]
      )
