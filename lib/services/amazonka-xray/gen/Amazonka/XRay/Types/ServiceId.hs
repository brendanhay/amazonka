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
-- Module      : Amazonka.XRay.Types.ServiceId
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ServiceId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newServiceId' smart constructor.
data ServiceId = ServiceId'
  { name :: Prelude.Maybe Prelude.Text,
    type' :: Prelude.Maybe Prelude.Text,
    names :: Prelude.Maybe [Prelude.Text],
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'serviceId_name' -
--
-- 'type'', 'serviceId_type' -
--
-- 'names', 'serviceId_names' -
--
-- 'accountId', 'serviceId_accountId' -
newServiceId ::
  ServiceId
newServiceId =
  ServiceId'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      names = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- |
serviceId_name :: Lens.Lens' ServiceId (Prelude.Maybe Prelude.Text)
serviceId_name = Lens.lens (\ServiceId' {name} -> name) (\s@ServiceId' {} a -> s {name = a} :: ServiceId)

-- |
serviceId_type :: Lens.Lens' ServiceId (Prelude.Maybe Prelude.Text)
serviceId_type = Lens.lens (\ServiceId' {type'} -> type') (\s@ServiceId' {} a -> s {type' = a} :: ServiceId)

-- |
serviceId_names :: Lens.Lens' ServiceId (Prelude.Maybe [Prelude.Text])
serviceId_names = Lens.lens (\ServiceId' {names} -> names) (\s@ServiceId' {} a -> s {names = a} :: ServiceId) Prelude.. Lens.mapping Lens.coerced

-- |
serviceId_accountId :: Lens.Lens' ServiceId (Prelude.Maybe Prelude.Text)
serviceId_accountId = Lens.lens (\ServiceId' {accountId} -> accountId) (\s@ServiceId' {} a -> s {accountId = a} :: ServiceId)

instance Core.FromJSON ServiceId where
  parseJSON =
    Core.withObject
      "ServiceId"
      ( \x ->
          ServiceId'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Names" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AccountId")
      )

instance Prelude.Hashable ServiceId where
  hashWithSalt _salt ServiceId' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData ServiceId where
  rnf ServiceId' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf accountId
