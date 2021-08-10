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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateEnclaveOptionsRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves. For
-- more information, see
-- <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave.html What is AWS Nitro Enclaves?>
-- in the /AWS Nitro Enclaves User Guide/.
--
-- /See:/ 'newLaunchTemplateEnclaveOptionsRequest' smart constructor.
data LaunchTemplateEnclaveOptionsRequest = LaunchTemplateEnclaveOptionsRequest'
  { -- | To enable the instance for AWS Nitro Enclaves, set this parameter to
    -- @true@.
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
-- 'enabled', 'launchTemplateEnclaveOptionsRequest_enabled' - To enable the instance for AWS Nitro Enclaves, set this parameter to
-- @true@.
newLaunchTemplateEnclaveOptionsRequest ::
  LaunchTemplateEnclaveOptionsRequest
newLaunchTemplateEnclaveOptionsRequest =
  LaunchTemplateEnclaveOptionsRequest'
    { enabled =
        Prelude.Nothing
    }

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to
-- @true@.
launchTemplateEnclaveOptionsRequest_enabled :: Lens.Lens' LaunchTemplateEnclaveOptionsRequest (Prelude.Maybe Prelude.Bool)
launchTemplateEnclaveOptionsRequest_enabled = Lens.lens (\LaunchTemplateEnclaveOptionsRequest' {enabled} -> enabled) (\s@LaunchTemplateEnclaveOptionsRequest' {} a -> s {enabled = a} :: LaunchTemplateEnclaveOptionsRequest)

instance
  Prelude.Hashable
    LaunchTemplateEnclaveOptionsRequest

instance
  Prelude.NFData
    LaunchTemplateEnclaveOptionsRequest

instance
  Core.ToQuery
    LaunchTemplateEnclaveOptionsRequest
  where
  toQuery LaunchTemplateEnclaveOptionsRequest' {..} =
    Prelude.mconcat ["Enabled" Core.=: enabled]
