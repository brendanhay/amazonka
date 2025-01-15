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
-- Module      : Amazonka.EC2.Types.AddedPrincipal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AddedPrincipal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PrincipalType
import qualified Amazonka.Prelude as Prelude

-- | Describes a principal.
--
-- /See:/ 'newAddedPrincipal' smart constructor.
data AddedPrincipal = AddedPrincipal'
  { -- | The Amazon Resource Name (ARN) of the principal.
    principal :: Prelude.Maybe Prelude.Text,
    -- | The type of principal.
    principalType :: Prelude.Maybe PrincipalType,
    -- | The ID of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service permission.
    servicePermissionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddedPrincipal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'addedPrincipal_principal' - The Amazon Resource Name (ARN) of the principal.
--
-- 'principalType', 'addedPrincipal_principalType' - The type of principal.
--
-- 'serviceId', 'addedPrincipal_serviceId' - The ID of the service.
--
-- 'servicePermissionId', 'addedPrincipal_servicePermissionId' - The ID of the service permission.
newAddedPrincipal ::
  AddedPrincipal
newAddedPrincipal =
  AddedPrincipal'
    { principal = Prelude.Nothing,
      principalType = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      servicePermissionId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the principal.
addedPrincipal_principal :: Lens.Lens' AddedPrincipal (Prelude.Maybe Prelude.Text)
addedPrincipal_principal = Lens.lens (\AddedPrincipal' {principal} -> principal) (\s@AddedPrincipal' {} a -> s {principal = a} :: AddedPrincipal)

-- | The type of principal.
addedPrincipal_principalType :: Lens.Lens' AddedPrincipal (Prelude.Maybe PrincipalType)
addedPrincipal_principalType = Lens.lens (\AddedPrincipal' {principalType} -> principalType) (\s@AddedPrincipal' {} a -> s {principalType = a} :: AddedPrincipal)

-- | The ID of the service.
addedPrincipal_serviceId :: Lens.Lens' AddedPrincipal (Prelude.Maybe Prelude.Text)
addedPrincipal_serviceId = Lens.lens (\AddedPrincipal' {serviceId} -> serviceId) (\s@AddedPrincipal' {} a -> s {serviceId = a} :: AddedPrincipal)

-- | The ID of the service permission.
addedPrincipal_servicePermissionId :: Lens.Lens' AddedPrincipal (Prelude.Maybe Prelude.Text)
addedPrincipal_servicePermissionId = Lens.lens (\AddedPrincipal' {servicePermissionId} -> servicePermissionId) (\s@AddedPrincipal' {} a -> s {servicePermissionId = a} :: AddedPrincipal)

instance Data.FromXML AddedPrincipal where
  parseXML x =
    AddedPrincipal'
      Prelude.<$> (x Data..@? "principal")
      Prelude.<*> (x Data..@? "principalType")
      Prelude.<*> (x Data..@? "serviceId")
      Prelude.<*> (x Data..@? "servicePermissionId")

instance Prelude.Hashable AddedPrincipal where
  hashWithSalt _salt AddedPrincipal' {..} =
    _salt
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` principalType
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` servicePermissionId

instance Prelude.NFData AddedPrincipal where
  rnf AddedPrincipal' {..} =
    Prelude.rnf principal `Prelude.seq`
      Prelude.rnf principalType `Prelude.seq`
        Prelude.rnf serviceId `Prelude.seq`
          Prelude.rnf servicePermissionId
