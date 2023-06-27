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
-- Module      : Amazonka.EC2.Types.DeviceOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DeviceOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for an Amazon Web Services Verified Access
-- device-identity based trust provider.
--
-- /See:/ 'newDeviceOptions' smart constructor.
data DeviceOptions = DeviceOptions'
  { -- | The ID of the tenant application with the device-identity provider.
    tenantId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tenantId', 'deviceOptions_tenantId' - The ID of the tenant application with the device-identity provider.
newDeviceOptions ::
  DeviceOptions
newDeviceOptions =
  DeviceOptions' {tenantId = Prelude.Nothing}

-- | The ID of the tenant application with the device-identity provider.
deviceOptions_tenantId :: Lens.Lens' DeviceOptions (Prelude.Maybe Prelude.Text)
deviceOptions_tenantId = Lens.lens (\DeviceOptions' {tenantId} -> tenantId) (\s@DeviceOptions' {} a -> s {tenantId = a} :: DeviceOptions)

instance Data.FromXML DeviceOptions where
  parseXML x =
    DeviceOptions' Prelude.<$> (x Data..@? "tenantId")

instance Prelude.Hashable DeviceOptions where
  hashWithSalt _salt DeviceOptions' {..} =
    _salt `Prelude.hashWithSalt` tenantId

instance Prelude.NFData DeviceOptions where
  rnf DeviceOptions' {..} = Prelude.rnf tenantId
