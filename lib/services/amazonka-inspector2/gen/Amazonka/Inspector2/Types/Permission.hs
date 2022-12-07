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
-- Module      : Amazonka.Inspector2.Types.Permission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Permission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Operation
import Amazonka.Inspector2.Types.Service
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the permissions an account has within Amazon
-- Inspector.
--
-- /See:/ 'newPermission' smart constructor.
data Permission = Permission'
  { -- | The operations that can be performed with the given permissions.
    operation :: Operation,
    -- | The services that the permissions allow an account to perform the given
    -- operations for.
    service :: Service
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Permission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'permission_operation' - The operations that can be performed with the given permissions.
--
-- 'service', 'permission_service' - The services that the permissions allow an account to perform the given
-- operations for.
newPermission ::
  -- | 'operation'
  Operation ->
  -- | 'service'
  Service ->
  Permission
newPermission pOperation_ pService_ =
  Permission'
    { operation = pOperation_,
      service = pService_
    }

-- | The operations that can be performed with the given permissions.
permission_operation :: Lens.Lens' Permission Operation
permission_operation = Lens.lens (\Permission' {operation} -> operation) (\s@Permission' {} a -> s {operation = a} :: Permission)

-- | The services that the permissions allow an account to perform the given
-- operations for.
permission_service :: Lens.Lens' Permission Service
permission_service = Lens.lens (\Permission' {service} -> service) (\s@Permission' {} a -> s {service = a} :: Permission)

instance Data.FromJSON Permission where
  parseJSON =
    Data.withObject
      "Permission"
      ( \x ->
          Permission'
            Prelude.<$> (x Data..: "operation")
            Prelude.<*> (x Data..: "service")
      )

instance Prelude.Hashable Permission where
  hashWithSalt _salt Permission' {..} =
    _salt `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` service

instance Prelude.NFData Permission where
  rnf Permission' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf service
