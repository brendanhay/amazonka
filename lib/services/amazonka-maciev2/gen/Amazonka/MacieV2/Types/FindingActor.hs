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
-- Module      : Amazonka.MacieV2.Types.FindingActor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.FindingActor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.DomainDetails
import Amazonka.MacieV2.Types.IpAddressDetails
import Amazonka.MacieV2.Types.UserIdentity
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an entity that performed an action that
-- produced a policy finding for a resource.
--
-- /See:/ 'newFindingActor' smart constructor.
data FindingActor = FindingActor'
  { -- | The type and other characteristics of the entity that performed the
    -- action on the affected resource.
    userIdentity :: Prelude.Maybe UserIdentity,
    -- | The IP address of the device that the entity used to perform the action
    -- on the affected resource. This object also provides information such as
    -- the owner and geographic location for the IP address.
    ipAddressDetails :: Prelude.Maybe IpAddressDetails,
    -- | The domain name of the device that the entity used to perform the action
    -- on the affected resource.
    domainDetails :: Prelude.Maybe DomainDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingActor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userIdentity', 'findingActor_userIdentity' - The type and other characteristics of the entity that performed the
-- action on the affected resource.
--
-- 'ipAddressDetails', 'findingActor_ipAddressDetails' - The IP address of the device that the entity used to perform the action
-- on the affected resource. This object also provides information such as
-- the owner and geographic location for the IP address.
--
-- 'domainDetails', 'findingActor_domainDetails' - The domain name of the device that the entity used to perform the action
-- on the affected resource.
newFindingActor ::
  FindingActor
newFindingActor =
  FindingActor'
    { userIdentity = Prelude.Nothing,
      ipAddressDetails = Prelude.Nothing,
      domainDetails = Prelude.Nothing
    }

-- | The type and other characteristics of the entity that performed the
-- action on the affected resource.
findingActor_userIdentity :: Lens.Lens' FindingActor (Prelude.Maybe UserIdentity)
findingActor_userIdentity = Lens.lens (\FindingActor' {userIdentity} -> userIdentity) (\s@FindingActor' {} a -> s {userIdentity = a} :: FindingActor)

-- | The IP address of the device that the entity used to perform the action
-- on the affected resource. This object also provides information such as
-- the owner and geographic location for the IP address.
findingActor_ipAddressDetails :: Lens.Lens' FindingActor (Prelude.Maybe IpAddressDetails)
findingActor_ipAddressDetails = Lens.lens (\FindingActor' {ipAddressDetails} -> ipAddressDetails) (\s@FindingActor' {} a -> s {ipAddressDetails = a} :: FindingActor)

-- | The domain name of the device that the entity used to perform the action
-- on the affected resource.
findingActor_domainDetails :: Lens.Lens' FindingActor (Prelude.Maybe DomainDetails)
findingActor_domainDetails = Lens.lens (\FindingActor' {domainDetails} -> domainDetails) (\s@FindingActor' {} a -> s {domainDetails = a} :: FindingActor)

instance Core.FromJSON FindingActor where
  parseJSON =
    Core.withObject
      "FindingActor"
      ( \x ->
          FindingActor'
            Prelude.<$> (x Core..:? "userIdentity")
            Prelude.<*> (x Core..:? "ipAddressDetails")
            Prelude.<*> (x Core..:? "domainDetails")
      )

instance Prelude.Hashable FindingActor where
  hashWithSalt _salt FindingActor' {..} =
    _salt `Prelude.hashWithSalt` userIdentity
      `Prelude.hashWithSalt` ipAddressDetails
      `Prelude.hashWithSalt` domainDetails

instance Prelude.NFData FindingActor where
  rnf FindingActor' {..} =
    Prelude.rnf userIdentity
      `Prelude.seq` Prelude.rnf ipAddressDetails
      `Prelude.seq` Prelude.rnf domainDetails
