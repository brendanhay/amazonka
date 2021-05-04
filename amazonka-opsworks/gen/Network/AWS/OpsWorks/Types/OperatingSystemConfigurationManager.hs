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
-- Module      : Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A block that contains information about the configuration manager (Chef)
-- and the versions of the configuration manager that are supported for an
-- operating system.
--
-- /See:/ 'newOperatingSystemConfigurationManager' smart constructor.
data OperatingSystemConfigurationManager = OperatingSystemConfigurationManager'
  { -- | The versions of the configuration manager that are supported by an
    -- operating system.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration manager, which is Chef.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OperatingSystemConfigurationManager' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'operatingSystemConfigurationManager_version' - The versions of the configuration manager that are supported by an
-- operating system.
--
-- 'name', 'operatingSystemConfigurationManager_name' - The name of the configuration manager, which is Chef.
newOperatingSystemConfigurationManager ::
  OperatingSystemConfigurationManager
newOperatingSystemConfigurationManager =
  OperatingSystemConfigurationManager'
    { version =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The versions of the configuration manager that are supported by an
-- operating system.
operatingSystemConfigurationManager_version :: Lens.Lens' OperatingSystemConfigurationManager (Prelude.Maybe Prelude.Text)
operatingSystemConfigurationManager_version = Lens.lens (\OperatingSystemConfigurationManager' {version} -> version) (\s@OperatingSystemConfigurationManager' {} a -> s {version = a} :: OperatingSystemConfigurationManager)

-- | The name of the configuration manager, which is Chef.
operatingSystemConfigurationManager_name :: Lens.Lens' OperatingSystemConfigurationManager (Prelude.Maybe Prelude.Text)
operatingSystemConfigurationManager_name = Lens.lens (\OperatingSystemConfigurationManager' {name} -> name) (\s@OperatingSystemConfigurationManager' {} a -> s {name = a} :: OperatingSystemConfigurationManager)

instance
  Prelude.FromJSON
    OperatingSystemConfigurationManager
  where
  parseJSON =
    Prelude.withObject
      "OperatingSystemConfigurationManager"
      ( \x ->
          OperatingSystemConfigurationManager'
            Prelude.<$> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance
  Prelude.Hashable
    OperatingSystemConfigurationManager

instance
  Prelude.NFData
    OperatingSystemConfigurationManager
