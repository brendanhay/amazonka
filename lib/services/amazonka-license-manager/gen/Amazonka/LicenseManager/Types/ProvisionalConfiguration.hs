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
-- Module      : Amazonka.LicenseManager.Types.ProvisionalConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ProvisionalConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a provisional configuration.
--
-- /See:/ 'newProvisionalConfiguration' smart constructor.
data ProvisionalConfiguration = ProvisionalConfiguration'
  { -- | Maximum time for the provisional configuration, in minutes.
    maxTimeToLiveInMinutes :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionalConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxTimeToLiveInMinutes', 'provisionalConfiguration_maxTimeToLiveInMinutes' - Maximum time for the provisional configuration, in minutes.
newProvisionalConfiguration ::
  -- | 'maxTimeToLiveInMinutes'
  Prelude.Int ->
  ProvisionalConfiguration
newProvisionalConfiguration pMaxTimeToLiveInMinutes_ =
  ProvisionalConfiguration'
    { maxTimeToLiveInMinutes =
        pMaxTimeToLiveInMinutes_
    }

-- | Maximum time for the provisional configuration, in minutes.
provisionalConfiguration_maxTimeToLiveInMinutes :: Lens.Lens' ProvisionalConfiguration Prelude.Int
provisionalConfiguration_maxTimeToLiveInMinutes = Lens.lens (\ProvisionalConfiguration' {maxTimeToLiveInMinutes} -> maxTimeToLiveInMinutes) (\s@ProvisionalConfiguration' {} a -> s {maxTimeToLiveInMinutes = a} :: ProvisionalConfiguration)

instance Core.FromJSON ProvisionalConfiguration where
  parseJSON =
    Core.withObject
      "ProvisionalConfiguration"
      ( \x ->
          ProvisionalConfiguration'
            Prelude.<$> (x Core..: "MaxTimeToLiveInMinutes")
      )

instance Prelude.Hashable ProvisionalConfiguration where
  hashWithSalt _salt ProvisionalConfiguration' {..} =
    _salt `Prelude.hashWithSalt` maxTimeToLiveInMinutes

instance Prelude.NFData ProvisionalConfiguration where
  rnf ProvisionalConfiguration' {..} =
    Prelude.rnf maxTimeToLiveInMinutes

instance Core.ToJSON ProvisionalConfiguration where
  toJSON ProvisionalConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MaxTimeToLiveInMinutes"
                  Core..= maxTimeToLiveInMinutes
              )
          ]
      )
