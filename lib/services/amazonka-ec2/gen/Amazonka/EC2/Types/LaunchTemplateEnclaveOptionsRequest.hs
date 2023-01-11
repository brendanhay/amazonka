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
-- Module      : Amazonka.EC2.Types.LaunchTemplateEnclaveOptionsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateEnclaveOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether the instance is enabled for Amazon Web Services Nitro
-- Enclaves. For more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is Amazon Web Services Nitro Enclaves?>
-- in the /Amazon Web Services Nitro Enclaves User Guide/.
--
-- /See:/ 'newLaunchTemplateEnclaveOptionsRequest' smart constructor.
data LaunchTemplateEnclaveOptionsRequest = LaunchTemplateEnclaveOptionsRequest'
  { -- | To enable the instance for Amazon Web Services Nitro Enclaves, set this
    -- parameter to @true@.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateEnclaveOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'launchTemplateEnclaveOptionsRequest_enabled' - To enable the instance for Amazon Web Services Nitro Enclaves, set this
-- parameter to @true@.
newLaunchTemplateEnclaveOptionsRequest ::
  LaunchTemplateEnclaveOptionsRequest
newLaunchTemplateEnclaveOptionsRequest =
  LaunchTemplateEnclaveOptionsRequest'
    { enabled =
        Prelude.Nothing
    }

-- | To enable the instance for Amazon Web Services Nitro Enclaves, set this
-- parameter to @true@.
launchTemplateEnclaveOptionsRequest_enabled :: Lens.Lens' LaunchTemplateEnclaveOptionsRequest (Prelude.Maybe Prelude.Bool)
launchTemplateEnclaveOptionsRequest_enabled = Lens.lens (\LaunchTemplateEnclaveOptionsRequest' {enabled} -> enabled) (\s@LaunchTemplateEnclaveOptionsRequest' {} a -> s {enabled = a} :: LaunchTemplateEnclaveOptionsRequest)

instance
  Prelude.Hashable
    LaunchTemplateEnclaveOptionsRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateEnclaveOptionsRequest' {..} =
      _salt `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    LaunchTemplateEnclaveOptionsRequest
  where
  rnf LaunchTemplateEnclaveOptionsRequest' {..} =
    Prelude.rnf enabled

instance
  Data.ToQuery
    LaunchTemplateEnclaveOptionsRequest
  where
  toQuery LaunchTemplateEnclaveOptionsRequest' {..} =
    Prelude.mconcat ["Enabled" Data.=: enabled]
