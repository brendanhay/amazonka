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
-- Module      : Amazonka.KeySpaces.Types.ReplicationSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.ReplicationSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types.Rs
import qualified Amazonka.Prelude as Prelude

-- | The replication specification of the keyspace includes:
--
-- -   @regionList@ - up to six Amazon Web Services Regions where the
--     keyspace is replicated in.
--
-- -   @replicationStrategy@ - the required value is @SINGLE_REGION@ or
--     @MULTI_REGION@.
--
-- /See:/ 'newReplicationSpecification' smart constructor.
data ReplicationSpecification = ReplicationSpecification'
  { -- | The @regionList@ can contain up to six Amazon Web Services Regions where
    -- the keyspace is replicated in.
    regionList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The @replicationStrategy@ of a keyspace, the required value is
    -- @SINGLE_REGION@ or @MULTI_REGION@.
    replicationStrategy :: Rs
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionList', 'replicationSpecification_regionList' - The @regionList@ can contain up to six Amazon Web Services Regions where
-- the keyspace is replicated in.
--
-- 'replicationStrategy', 'replicationSpecification_replicationStrategy' - The @replicationStrategy@ of a keyspace, the required value is
-- @SINGLE_REGION@ or @MULTI_REGION@.
newReplicationSpecification ::
  -- | 'replicationStrategy'
  Rs ->
  ReplicationSpecification
newReplicationSpecification pReplicationStrategy_ =
  ReplicationSpecification'
    { regionList =
        Prelude.Nothing,
      replicationStrategy = pReplicationStrategy_
    }

-- | The @regionList@ can contain up to six Amazon Web Services Regions where
-- the keyspace is replicated in.
replicationSpecification_regionList :: Lens.Lens' ReplicationSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
replicationSpecification_regionList = Lens.lens (\ReplicationSpecification' {regionList} -> regionList) (\s@ReplicationSpecification' {} a -> s {regionList = a} :: ReplicationSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The @replicationStrategy@ of a keyspace, the required value is
-- @SINGLE_REGION@ or @MULTI_REGION@.
replicationSpecification_replicationStrategy :: Lens.Lens' ReplicationSpecification Rs
replicationSpecification_replicationStrategy = Lens.lens (\ReplicationSpecification' {replicationStrategy} -> replicationStrategy) (\s@ReplicationSpecification' {} a -> s {replicationStrategy = a} :: ReplicationSpecification)

instance Prelude.Hashable ReplicationSpecification where
  hashWithSalt _salt ReplicationSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` regionList
      `Prelude.hashWithSalt` replicationStrategy

instance Prelude.NFData ReplicationSpecification where
  rnf ReplicationSpecification' {..} =
    Prelude.rnf regionList
      `Prelude.seq` Prelude.rnf replicationStrategy

instance Data.ToJSON ReplicationSpecification where
  toJSON ReplicationSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("regionList" Data..=) Prelude.<$> regionList,
            Prelude.Just
              ("replicationStrategy" Data..= replicationStrategy)
          ]
      )
