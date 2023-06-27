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
-- Module      : Amazonka.EC2.Types.CreateVerifiedAccessTrustProviderDeviceOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreateVerifiedAccessTrustProviderDeviceOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the options when creating an Amazon Web Services Verified
-- Access trust provider using the @device@ type.
--
-- /See:/ 'newCreateVerifiedAccessTrustProviderDeviceOptions' smart constructor.
data CreateVerifiedAccessTrustProviderDeviceOptions = CreateVerifiedAccessTrustProviderDeviceOptions'
  { -- | The ID of the tenant application with the device-identity provider.
    tenantId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVerifiedAccessTrustProviderDeviceOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tenantId', 'createVerifiedAccessTrustProviderDeviceOptions_tenantId' - The ID of the tenant application with the device-identity provider.
newCreateVerifiedAccessTrustProviderDeviceOptions ::
  CreateVerifiedAccessTrustProviderDeviceOptions
newCreateVerifiedAccessTrustProviderDeviceOptions =
  CreateVerifiedAccessTrustProviderDeviceOptions'
    { tenantId =
        Prelude.Nothing
    }

-- | The ID of the tenant application with the device-identity provider.
createVerifiedAccessTrustProviderDeviceOptions_tenantId :: Lens.Lens' CreateVerifiedAccessTrustProviderDeviceOptions (Prelude.Maybe Prelude.Text)
createVerifiedAccessTrustProviderDeviceOptions_tenantId = Lens.lens (\CreateVerifiedAccessTrustProviderDeviceOptions' {tenantId} -> tenantId) (\s@CreateVerifiedAccessTrustProviderDeviceOptions' {} a -> s {tenantId = a} :: CreateVerifiedAccessTrustProviderDeviceOptions)

instance
  Prelude.Hashable
    CreateVerifiedAccessTrustProviderDeviceOptions
  where
  hashWithSalt
    _salt
    CreateVerifiedAccessTrustProviderDeviceOptions' {..} =
      _salt `Prelude.hashWithSalt` tenantId

instance
  Prelude.NFData
    CreateVerifiedAccessTrustProviderDeviceOptions
  where
  rnf
    CreateVerifiedAccessTrustProviderDeviceOptions' {..} =
      Prelude.rnf tenantId

instance
  Data.ToQuery
    CreateVerifiedAccessTrustProviderDeviceOptions
  where
  toQuery
    CreateVerifiedAccessTrustProviderDeviceOptions' {..} =
      Prelude.mconcat ["TenantId" Data.=: tenantId]
