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
-- Module      : Network.AWS.XRay.Types.ServiceId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ServiceId where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- |
--
-- /See:/ 'newServiceId' smart constructor.
data ServiceId = ServiceId'
  { names :: Prelude.Maybe [Prelude.Text],
    accountId :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Maybe Prelude.Text,
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { names = Prelude.Nothing,
      accountId = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- |
serviceId_names :: Lens.Lens' ServiceId (Prelude.Maybe [Prelude.Text])
serviceId_names = Lens.lens (\ServiceId' {names} -> names) (\s@ServiceId' {} a -> s {names = a} :: ServiceId) Prelude.. Lens.mapping Prelude._Coerce

-- |
serviceId_accountId :: Lens.Lens' ServiceId (Prelude.Maybe Prelude.Text)
serviceId_accountId = Lens.lens (\ServiceId' {accountId} -> accountId) (\s@ServiceId' {} a -> s {accountId = a} :: ServiceId)

-- |
serviceId_name :: Lens.Lens' ServiceId (Prelude.Maybe Prelude.Text)
serviceId_name = Lens.lens (\ServiceId' {name} -> name) (\s@ServiceId' {} a -> s {name = a} :: ServiceId)

-- |
serviceId_type :: Lens.Lens' ServiceId (Prelude.Maybe Prelude.Text)
serviceId_type = Lens.lens (\ServiceId' {type'} -> type') (\s@ServiceId' {} a -> s {type' = a} :: ServiceId)

instance Prelude.FromJSON ServiceId where
  parseJSON =
    Prelude.withObject
      "ServiceId"
      ( \x ->
          ServiceId'
            Prelude.<$> (x Prelude..:? "Names" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "AccountId")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable ServiceId

instance Prelude.NFData ServiceId
