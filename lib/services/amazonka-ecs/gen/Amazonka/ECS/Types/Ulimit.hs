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
-- Module      : Amazonka.ECS.Types.Ulimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Ulimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.UlimitName
import qualified Amazonka.Prelude as Prelude

-- | The @ulimit@ settings to pass to the container.
--
-- Amazon ECS tasks hosted on Fargate use the default resource limit values
-- set by the operating system with the exception of the @nofile@ resource
-- limit parameter which Fargate overrides. The @nofile@ resource limit
-- sets a restriction on the number of open files that a container can use.
-- The default @nofile@ soft limit is @1024@ and hard limit is @4096@.
--
-- /See:/ 'newUlimit' smart constructor.
data Ulimit = Ulimit'
  { -- | The @type@ of the @ulimit@.
    name :: UlimitName,
    -- | The soft limit for the @ulimit@ type.
    softLimit :: Prelude.Int,
    -- | The hard limit for the @ulimit@ type.
    hardLimit :: Prelude.Int
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
-- 'name', 'ulimit_name' - The @type@ of the @ulimit@.
--
-- 'softLimit', 'ulimit_softLimit' - The soft limit for the @ulimit@ type.
--
-- 'hardLimit', 'ulimit_hardLimit' - The hard limit for the @ulimit@ type.
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

-- | The soft limit for the @ulimit@ type.
ulimit_softLimit :: Lens.Lens' Ulimit Prelude.Int
ulimit_softLimit = Lens.lens (\Ulimit' {softLimit} -> softLimit) (\s@Ulimit' {} a -> s {softLimit = a} :: Ulimit)

-- | The hard limit for the @ulimit@ type.
ulimit_hardLimit :: Lens.Lens' Ulimit Prelude.Int
ulimit_hardLimit = Lens.lens (\Ulimit' {hardLimit} -> hardLimit) (\s@Ulimit' {} a -> s {hardLimit = a} :: Ulimit)

instance Data.FromJSON Ulimit where
  parseJSON =
    Data.withObject
      "Ulimit"
      ( \x ->
          Ulimit'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "softLimit")
            Prelude.<*> (x Data..: "hardLimit")
      )

instance Prelude.Hashable Ulimit where
  hashWithSalt _salt Ulimit' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` softLimit
      `Prelude.hashWithSalt` hardLimit

instance Prelude.NFData Ulimit where
  rnf Ulimit' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf softLimit
      `Prelude.seq` Prelude.rnf hardLimit

instance Data.ToJSON Ulimit where
  toJSON Ulimit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("softLimit" Data..= softLimit),
            Prelude.Just ("hardLimit" Data..= hardLimit)
          ]
      )
