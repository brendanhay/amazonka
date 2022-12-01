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
-- Module      : Amazonka.EC2.Types.LaunchTemplateEnclaveOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateEnclaveOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether the instance is enabled for Amazon Web Services Nitro
-- Enclaves.
--
-- /See:/ 'newLaunchTemplateEnclaveOptions' smart constructor.
data LaunchTemplateEnclaveOptions = LaunchTemplateEnclaveOptions'
  { -- | If this parameter is set to @true@, the instance is enabled for Amazon
    -- Web Services Nitro Enclaves; otherwise, it is not enabled for Amazon Web
    -- Services Nitro Enclaves.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateEnclaveOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'launchTemplateEnclaveOptions_enabled' - If this parameter is set to @true@, the instance is enabled for Amazon
-- Web Services Nitro Enclaves; otherwise, it is not enabled for Amazon Web
-- Services Nitro Enclaves.
newLaunchTemplateEnclaveOptions ::
  LaunchTemplateEnclaveOptions
newLaunchTemplateEnclaveOptions =
  LaunchTemplateEnclaveOptions'
    { enabled =
        Prelude.Nothing
    }

-- | If this parameter is set to @true@, the instance is enabled for Amazon
-- Web Services Nitro Enclaves; otherwise, it is not enabled for Amazon Web
-- Services Nitro Enclaves.
launchTemplateEnclaveOptions_enabled :: Lens.Lens' LaunchTemplateEnclaveOptions (Prelude.Maybe Prelude.Bool)
launchTemplateEnclaveOptions_enabled = Lens.lens (\LaunchTemplateEnclaveOptions' {enabled} -> enabled) (\s@LaunchTemplateEnclaveOptions' {} a -> s {enabled = a} :: LaunchTemplateEnclaveOptions)

instance Core.FromXML LaunchTemplateEnclaveOptions where
  parseXML x =
    LaunchTemplateEnclaveOptions'
      Prelude.<$> (x Core..@? "enabled")

instance
  Prelude.Hashable
    LaunchTemplateEnclaveOptions
  where
  hashWithSalt _salt LaunchTemplateEnclaveOptions' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData LaunchTemplateEnclaveOptions where
  rnf LaunchTemplateEnclaveOptions' {..} =
    Prelude.rnf enabled
