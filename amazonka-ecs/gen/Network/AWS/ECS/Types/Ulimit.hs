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
-- Module      : Network.AWS.ECS.Types.Ulimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Ulimit where

import Network.AWS.ECS.Types.UlimitName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The @ulimit@ settings to pass to the container.
--
-- /See:/ 'newUlimit' smart constructor.
data Ulimit = Ulimit'
  { -- | The @type@ of the @ulimit@.
    name :: UlimitName,
    -- | The soft limit for the ulimit type.
    softLimit :: Prelude.Int,
    -- | The hard limit for the ulimit type.
    hardLimit :: Prelude.Int
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
-- 'name', 'ulimit_name' - The @type@ of the @ulimit@.
--
-- 'softLimit', 'ulimit_softLimit' - The soft limit for the ulimit type.
--
-- 'hardLimit', 'ulimit_hardLimit' - The hard limit for the ulimit type.
newUlimit ::
  -- | 'name'
  UlimitName ->
  -- | 'softLimit'
  Prelude.Int ->
  -- | 'hardLimit'
  Prelude.Int ->
  Ulimit
newUlimit pName_ pSoftLimit_ pHardLimit_ =
  Ulimit'
    { name = pName_,
      softLimit = pSoftLimit_,
      hardLimit = pHardLimit_
    }

-- | The @type@ of the @ulimit@.
ulimit_name :: Lens.Lens' Ulimit UlimitName
ulimit_name = Lens.lens (\Ulimit' {name} -> name) (\s@Ulimit' {} a -> s {name = a} :: Ulimit)

-- | The soft limit for the ulimit type.
ulimit_softLimit :: Lens.Lens' Ulimit Prelude.Int
ulimit_softLimit = Lens.lens (\Ulimit' {softLimit} -> softLimit) (\s@Ulimit' {} a -> s {softLimit = a} :: Ulimit)

-- | The hard limit for the ulimit type.
ulimit_hardLimit :: Lens.Lens' Ulimit Prelude.Int
ulimit_hardLimit = Lens.lens (\Ulimit' {hardLimit} -> hardLimit) (\s@Ulimit' {} a -> s {hardLimit = a} :: Ulimit)

instance Prelude.FromJSON Ulimit where
  parseJSON =
    Prelude.withObject
      "Ulimit"
      ( \x ->
          Ulimit'
            Prelude.<$> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "softLimit")
            Prelude.<*> (x Prelude..: "hardLimit")
      )

instance Prelude.Hashable Ulimit

instance Prelude.NFData Ulimit

instance Prelude.ToJSON Ulimit where
  toJSON Ulimit' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("softLimit" Prelude..= softLimit),
            Prelude.Just ("hardLimit" Prelude..= hardLimit)
          ]
      )
