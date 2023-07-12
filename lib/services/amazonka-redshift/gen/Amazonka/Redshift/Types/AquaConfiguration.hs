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
-- Module      : Amazonka.Redshift.Types.AquaConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.AquaConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.AquaConfigurationStatus
import Amazonka.Redshift.Types.AquaStatus

-- | The operation that uses this structure is retired. Amazon Redshift
-- automatically determines whether to use AQUA (Advanced Query
-- Accelerator).
--
-- /See:/ 'newAquaConfiguration' smart constructor.
data AquaConfiguration = AquaConfiguration'
  { -- | This field is retired. Amazon Redshift automatically determines whether
    -- to use AQUA (Advanced Query Accelerator).
    aquaConfigurationStatus :: Prelude.Maybe AquaConfigurationStatus,
    -- | This field is retired. Amazon Redshift automatically determines whether
    -- to use AQUA (Advanced Query Accelerator).
    aquaStatus :: Prelude.Maybe AquaStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AquaConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aquaConfigurationStatus', 'aquaConfiguration_aquaConfigurationStatus' - This field is retired. Amazon Redshift automatically determines whether
-- to use AQUA (Advanced Query Accelerator).
--
-- 'aquaStatus', 'aquaConfiguration_aquaStatus' - This field is retired. Amazon Redshift automatically determines whether
-- to use AQUA (Advanced Query Accelerator).
newAquaConfiguration ::
  AquaConfiguration
newAquaConfiguration =
  AquaConfiguration'
    { aquaConfigurationStatus =
        Prelude.Nothing,
      aquaStatus = Prelude.Nothing
    }

-- | This field is retired. Amazon Redshift automatically determines whether
-- to use AQUA (Advanced Query Accelerator).
aquaConfiguration_aquaConfigurationStatus :: Lens.Lens' AquaConfiguration (Prelude.Maybe AquaConfigurationStatus)
aquaConfiguration_aquaConfigurationStatus = Lens.lens (\AquaConfiguration' {aquaConfigurationStatus} -> aquaConfigurationStatus) (\s@AquaConfiguration' {} a -> s {aquaConfigurationStatus = a} :: AquaConfiguration)

-- | This field is retired. Amazon Redshift automatically determines whether
-- to use AQUA (Advanced Query Accelerator).
aquaConfiguration_aquaStatus :: Lens.Lens' AquaConfiguration (Prelude.Maybe AquaStatus)
aquaConfiguration_aquaStatus = Lens.lens (\AquaConfiguration' {aquaStatus} -> aquaStatus) (\s@AquaConfiguration' {} a -> s {aquaStatus = a} :: AquaConfiguration)

instance Data.FromXML AquaConfiguration where
  parseXML x =
    AquaConfiguration'
      Prelude.<$> (x Data..@? "AquaConfigurationStatus")
      Prelude.<*> (x Data..@? "AquaStatus")

instance Prelude.Hashable AquaConfiguration where
  hashWithSalt _salt AquaConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` aquaConfigurationStatus
      `Prelude.hashWithSalt` aquaStatus

instance Prelude.NFData AquaConfiguration where
  rnf AquaConfiguration' {..} =
    Prelude.rnf aquaConfigurationStatus
      `Prelude.seq` Prelude.rnf aquaStatus
