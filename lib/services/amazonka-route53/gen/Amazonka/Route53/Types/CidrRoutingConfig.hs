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
-- Module      : Amazonka.Route53.Types.CidrRoutingConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.CidrRoutingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | The object that is specified in resource record set object when you are
-- linking a resource record set to a CIDR location.
--
-- A @LocationName@ with an asterisk “*” can be used to create a default
-- CIDR record. @CollectionId@ is still required for default record.
--
-- /See:/ 'newCidrRoutingConfig' smart constructor.
data CidrRoutingConfig = CidrRoutingConfig'
  { -- | The CIDR collection ID.
    collectionId :: Prelude.Text,
    -- | The CIDR collection location name.
    locationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CidrRoutingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionId', 'cidrRoutingConfig_collectionId' - The CIDR collection ID.
--
-- 'locationName', 'cidrRoutingConfig_locationName' - The CIDR collection location name.
newCidrRoutingConfig ::
  -- | 'collectionId'
  Prelude.Text ->
  -- | 'locationName'
  Prelude.Text ->
  CidrRoutingConfig
newCidrRoutingConfig pCollectionId_ pLocationName_ =
  CidrRoutingConfig'
    { collectionId = pCollectionId_,
      locationName = pLocationName_
    }

-- | The CIDR collection ID.
cidrRoutingConfig_collectionId :: Lens.Lens' CidrRoutingConfig Prelude.Text
cidrRoutingConfig_collectionId = Lens.lens (\CidrRoutingConfig' {collectionId} -> collectionId) (\s@CidrRoutingConfig' {} a -> s {collectionId = a} :: CidrRoutingConfig)

-- | The CIDR collection location name.
cidrRoutingConfig_locationName :: Lens.Lens' CidrRoutingConfig Prelude.Text
cidrRoutingConfig_locationName = Lens.lens (\CidrRoutingConfig' {locationName} -> locationName) (\s@CidrRoutingConfig' {} a -> s {locationName = a} :: CidrRoutingConfig)

instance Core.FromXML CidrRoutingConfig where
  parseXML x =
    CidrRoutingConfig'
      Prelude.<$> (x Core..@ "CollectionId")
      Prelude.<*> (x Core..@ "LocationName")

instance Prelude.Hashable CidrRoutingConfig where
  hashWithSalt _salt CidrRoutingConfig' {..} =
    _salt `Prelude.hashWithSalt` collectionId
      `Prelude.hashWithSalt` locationName

instance Prelude.NFData CidrRoutingConfig where
  rnf CidrRoutingConfig' {..} =
    Prelude.rnf collectionId
      `Prelude.seq` Prelude.rnf locationName

instance Core.ToXML CidrRoutingConfig where
  toXML CidrRoutingConfig' {..} =
    Prelude.mconcat
      [ "CollectionId" Core.@= collectionId,
        "LocationName" Core.@= locationName
      ]
