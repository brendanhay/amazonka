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
-- Module      : Network.AWS.ECR.Types.ReplicationRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ReplicationRule where

import Network.AWS.ECR.Types.ReplicationDestination
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An array of objects representing the replication destinations for a
-- replication configuration. A replication configuration may contain only
-- one replication rule but the rule may contain one or more replication
-- destinations.
--
-- /See:/ 'newReplicationRule' smart constructor.
data ReplicationRule = ReplicationRule'
  { -- | An array of objects representing the details of a replication
    -- destination.
    destinations :: [ReplicationDestination]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplicationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinations', 'replicationRule_destinations' - An array of objects representing the details of a replication
-- destination.
newReplicationRule ::
  ReplicationRule
newReplicationRule =
  ReplicationRule' {destinations = Prelude.mempty}

-- | An array of objects representing the details of a replication
-- destination.
replicationRule_destinations :: Lens.Lens' ReplicationRule [ReplicationDestination]
replicationRule_destinations = Lens.lens (\ReplicationRule' {destinations} -> destinations) (\s@ReplicationRule' {} a -> s {destinations = a} :: ReplicationRule) Prelude.. Prelude._Coerce

instance Prelude.FromJSON ReplicationRule where
  parseJSON =
    Prelude.withObject
      "ReplicationRule"
      ( \x ->
          ReplicationRule'
            Prelude.<$> ( x Prelude..:? "destinations"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ReplicationRule

instance Prelude.NFData ReplicationRule

instance Prelude.ToJSON ReplicationRule where
  toJSON ReplicationRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("destinations" Prelude..= destinations)
          ]
      )
