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
-- Module      : Amazonka.Nimble.Types.ComputeFarmConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.ComputeFarmConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a render farm that is associated with a studio
-- resource.
--
-- /See:/ 'newComputeFarmConfiguration' smart constructor.
data ComputeFarmConfiguration = ComputeFarmConfiguration'
  { -- | The name of an Active Directory user that is used on ComputeFarm worker
    -- instances.
    activeDirectoryUser :: Prelude.Maybe Prelude.Text,
    -- | The endpoint of the ComputeFarm that is accessed by the studio component
    -- resource.
    endpoint :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComputeFarmConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryUser', 'computeFarmConfiguration_activeDirectoryUser' - The name of an Active Directory user that is used on ComputeFarm worker
-- instances.
--
-- 'endpoint', 'computeFarmConfiguration_endpoint' - The endpoint of the ComputeFarm that is accessed by the studio component
-- resource.
newComputeFarmConfiguration ::
  ComputeFarmConfiguration
newComputeFarmConfiguration =
  ComputeFarmConfiguration'
    { activeDirectoryUser =
        Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | The name of an Active Directory user that is used on ComputeFarm worker
-- instances.
computeFarmConfiguration_activeDirectoryUser :: Lens.Lens' ComputeFarmConfiguration (Prelude.Maybe Prelude.Text)
computeFarmConfiguration_activeDirectoryUser = Lens.lens (\ComputeFarmConfiguration' {activeDirectoryUser} -> activeDirectoryUser) (\s@ComputeFarmConfiguration' {} a -> s {activeDirectoryUser = a} :: ComputeFarmConfiguration)

-- | The endpoint of the ComputeFarm that is accessed by the studio component
-- resource.
computeFarmConfiguration_endpoint :: Lens.Lens' ComputeFarmConfiguration (Prelude.Maybe Prelude.Text)
computeFarmConfiguration_endpoint = Lens.lens (\ComputeFarmConfiguration' {endpoint} -> endpoint) (\s@ComputeFarmConfiguration' {} a -> s {endpoint = a} :: ComputeFarmConfiguration) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON ComputeFarmConfiguration where
  parseJSON =
    Core.withObject
      "ComputeFarmConfiguration"
      ( \x ->
          ComputeFarmConfiguration'
            Prelude.<$> (x Core..:? "activeDirectoryUser")
            Prelude.<*> (x Core..:? "endpoint")
      )

instance Prelude.Hashable ComputeFarmConfiguration where
  hashWithSalt _salt ComputeFarmConfiguration' {..} =
    _salt `Prelude.hashWithSalt` activeDirectoryUser
      `Prelude.hashWithSalt` endpoint

instance Prelude.NFData ComputeFarmConfiguration where
  rnf ComputeFarmConfiguration' {..} =
    Prelude.rnf activeDirectoryUser
      `Prelude.seq` Prelude.rnf endpoint

instance Core.ToJSON ComputeFarmConfiguration where
  toJSON ComputeFarmConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("activeDirectoryUser" Core..=)
              Prelude.<$> activeDirectoryUser,
            ("endpoint" Core..=) Prelude.<$> endpoint
          ]
      )
