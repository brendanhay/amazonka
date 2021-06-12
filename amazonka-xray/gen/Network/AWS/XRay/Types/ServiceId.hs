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
-- Module      : Network.AWS.XRay.Types.ServiceId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ServiceId where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- |
--
-- /See:/ 'newServiceId' smart constructor.
data ServiceId = ServiceId'
  { names :: Core.Maybe [Core.Text],
    accountId :: Core.Maybe Core.Text,
    name :: Core.Maybe Core.Text,
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'serviceId_names' -
--
-- 'accountId', 'serviceId_accountId' -
--
-- 'name', 'serviceId_name' -
--
-- 'type'', 'serviceId_type' -
newServiceId ::
  ServiceId
newServiceId =
  ServiceId'
    { names = Core.Nothing,
      accountId = Core.Nothing,
      name = Core.Nothing,
      type' = Core.Nothing
    }

-- |
serviceId_names :: Lens.Lens' ServiceId (Core.Maybe [Core.Text])
serviceId_names = Lens.lens (\ServiceId' {names} -> names) (\s@ServiceId' {} a -> s {names = a} :: ServiceId) Core.. Lens.mapping Lens._Coerce

-- |
serviceId_accountId :: Lens.Lens' ServiceId (Core.Maybe Core.Text)
serviceId_accountId = Lens.lens (\ServiceId' {accountId} -> accountId) (\s@ServiceId' {} a -> s {accountId = a} :: ServiceId)

-- |
serviceId_name :: Lens.Lens' ServiceId (Core.Maybe Core.Text)
serviceId_name = Lens.lens (\ServiceId' {name} -> name) (\s@ServiceId' {} a -> s {name = a} :: ServiceId)

-- |
serviceId_type :: Lens.Lens' ServiceId (Core.Maybe Core.Text)
serviceId_type = Lens.lens (\ServiceId' {type'} -> type') (\s@ServiceId' {} a -> s {type' = a} :: ServiceId)

instance Core.FromJSON ServiceId where
  parseJSON =
    Core.withObject
      "ServiceId"
      ( \x ->
          ServiceId'
            Core.<$> (x Core..:? "Names" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AccountId")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable ServiceId

instance Core.NFData ServiceId
