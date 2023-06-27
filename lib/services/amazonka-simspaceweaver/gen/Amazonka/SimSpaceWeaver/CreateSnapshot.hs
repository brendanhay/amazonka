{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SimSpaceWeaver.CreateSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of the specified simulation. A snapshot is a file
-- that contains simulation state data at a specific time. The state data
-- saved in a snapshot includes entity data from the State Fabric, the
-- simulation configuration specified in the schema, and the clock tick
-- number. You can use the snapshot to initialize a new simulation. For
-- more information about snapshots, see
-- <https://docs.aws.amazon.com/simspaceweaver/latest/userguide/working-with_snapshots.html Snapshots>
-- in the /SimSpace Weaver User Guide/.
--
-- You specify a @Destination@ when you create a snapshot. The
-- @Destination@ is the name of an Amazon S3 bucket and an optional
-- @ObjectKeyPrefix@. The @ObjectKeyPrefix@ is usually the name of a folder
-- in the bucket. SimSpace Weaver creates a @snapshot@ folder inside the
-- @Destination@ and places the snapshot file there.
--
-- The snapshot file is an Amazon S3 object. It has an object key with the
-- form:
-- @ @/@object-key-prefix@/@\/snapshot\/@/@simulation-name@/@-@/@YYMMdd@/@-@/@HHmm@/@-@/@ss@/@.zip@,
-- where:
--
-- -   @ @/@YY@/@ @ is the 2-digit year
--
-- -   @ @/@MM@/@ @ is the 2-digit month
--
-- -   @ @/@dd@/@ @ is the 2-digit day of the month
--
-- -   @ @/@HH@/@ @ is the 2-digit hour (24-hour clock)
--
-- -   @ @/@mm@/@ @ is the 2-digit minutes
--
-- -   @ @/@ss@/@ @ is the 2-digit seconds
module Amazonka.SimSpaceWeaver.CreateSnapshot
  ( -- * Creating a Request
    CreateSnapshot (..),
    newCreateSnapshot,

    -- * Request Lenses
    createSnapshot_destination,
    createSnapshot_simulation,

    -- * Destructuring the Response
    CreateSnapshotResponse (..),
    newCreateSnapshotResponse,

    -- * Response Lenses
    createSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | The Amazon S3 bucket and optional folder (object key prefix) where
    -- SimSpace Weaver creates the snapshot file.
    --
    -- The Amazon S3 bucket must be in the same Amazon Web Services Region as
    -- the simulation.
    destination :: S3Destination,
    -- | The name of the simulation.
    simulation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'createSnapshot_destination' - The Amazon S3 bucket and optional folder (object key prefix) where
-- SimSpace Weaver creates the snapshot file.
--
-- The Amazon S3 bucket must be in the same Amazon Web Services Region as
-- the simulation.
--
-- 'simulation', 'createSnapshot_simulation' - The name of the simulation.
newCreateSnapshot ::
  -- | 'destination'
  S3Destination ->
  -- | 'simulation'
  Prelude.Text ->
  CreateSnapshot
newCreateSnapshot pDestination_ pSimulation_ =
  CreateSnapshot'
    { destination = pDestination_,
      simulation = pSimulation_
    }

-- | The Amazon S3 bucket and optional folder (object key prefix) where
-- SimSpace Weaver creates the snapshot file.
--
-- The Amazon S3 bucket must be in the same Amazon Web Services Region as
-- the simulation.
createSnapshot_destination :: Lens.Lens' CreateSnapshot S3Destination
createSnapshot_destination = Lens.lens (\CreateSnapshot' {destination} -> destination) (\s@CreateSnapshot' {} a -> s {destination = a} :: CreateSnapshot)

-- | The name of the simulation.
createSnapshot_simulation :: Lens.Lens' CreateSnapshot Prelude.Text
createSnapshot_simulation = Lens.lens (\CreateSnapshot' {simulation} -> simulation) (\s@CreateSnapshot' {} a -> s {simulation = a} :: CreateSnapshot)

instance Core.AWSRequest CreateSnapshot where
  type
    AWSResponse CreateSnapshot =
      CreateSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateSnapshotResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSnapshot where
  hashWithSalt _salt CreateSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` simulation

instance Prelude.NFData CreateSnapshot where
  rnf CreateSnapshot' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf simulation

instance Data.ToHeaders CreateSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSnapshot where
  toJSON CreateSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Destination" Data..= destination),
            Prelude.Just ("Simulation" Data..= simulation)
          ]
      )

instance Data.ToPath CreateSnapshot where
  toPath = Prelude.const "/createsnapshot"

instance Data.ToQuery CreateSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSnapshotResponse_httpStatus' - The response's http status code.
newCreateSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSnapshotResponse
newCreateSnapshotResponse pHttpStatus_ =
  CreateSnapshotResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createSnapshotResponse_httpStatus :: Lens.Lens' CreateSnapshotResponse Prelude.Int
createSnapshotResponse_httpStatus = Lens.lens (\CreateSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotResponse' {} a -> s {httpStatus = a} :: CreateSnapshotResponse)

instance Prelude.NFData CreateSnapshotResponse where
  rnf CreateSnapshotResponse' {..} =
    Prelude.rnf httpStatus
