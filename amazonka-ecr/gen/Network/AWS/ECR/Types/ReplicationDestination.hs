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
-- Module      : Network.AWS.ECR.Types.ReplicationDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ReplicationDestination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An array of objects representing the details of a replication
-- destination.
--
-- /See:/ 'newReplicationDestination' smart constructor.
data ReplicationDestination = ReplicationDestination'
  { -- | A Region to replicate to.
    region :: Prelude.Text,
    -- | The account ID of the destination registry to replicate to.
    registryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplicationDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'region', 'replicationDestination_region' - A Region to replicate to.
--
-- 'registryId', 'replicationDestination_registryId' - The account ID of the destination registry to replicate to.
newReplicationDestination ::
  -- | 'region'
  Prelude.Text ->
  -- | 'registryId'
  Prelude.Text ->
  ReplicationDestination
newReplicationDestination pRegion_ pRegistryId_ =
  ReplicationDestination'
    { region = pRegion_,
      registryId = pRegistryId_
    }

-- | A Region to replicate to.
replicationDestination_region :: Lens.Lens' ReplicationDestination Prelude.Text
replicationDestination_region = Lens.lens (\ReplicationDestination' {region} -> region) (\s@ReplicationDestination' {} a -> s {region = a} :: ReplicationDestination)

-- | The account ID of the destination registry to replicate to.
replicationDestination_registryId :: Lens.Lens' ReplicationDestination Prelude.Text
replicationDestination_registryId = Lens.lens (\ReplicationDestination' {registryId} -> registryId) (\s@ReplicationDestination' {} a -> s {registryId = a} :: ReplicationDestination)

instance Prelude.FromJSON ReplicationDestination where
  parseJSON =
    Prelude.withObject
      "ReplicationDestination"
      ( \x ->
          ReplicationDestination'
            Prelude.<$> (x Prelude..: "region")
            Prelude.<*> (x Prelude..: "registryId")
      )

instance Prelude.Hashable ReplicationDestination

instance Prelude.NFData ReplicationDestination

instance Prelude.ToJSON ReplicationDestination where
  toJSON ReplicationDestination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("region" Prelude..= region),
            Prelude.Just ("registryId" Prelude..= registryId)
          ]
      )
