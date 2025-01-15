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
-- Module      : Amazonka.GroundStation.DescribeEphemeris
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing ephemeris.
module Amazonka.GroundStation.DescribeEphemeris
  ( -- * Creating a Request
    DescribeEphemeris (..),
    newDescribeEphemeris,

    -- * Request Lenses
    describeEphemeris_ephemerisId,

    -- * Destructuring the Response
    DescribeEphemerisResponse (..),
    newDescribeEphemerisResponse,

    -- * Response Lenses
    describeEphemerisResponse_creationTime,
    describeEphemerisResponse_enabled,
    describeEphemerisResponse_ephemerisId,
    describeEphemerisResponse_invalidReason,
    describeEphemerisResponse_name,
    describeEphemerisResponse_priority,
    describeEphemerisResponse_satelliteId,
    describeEphemerisResponse_status,
    describeEphemerisResponse_suppliedData,
    describeEphemerisResponse_tags,
    describeEphemerisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEphemeris' smart constructor.
data DescribeEphemeris = DescribeEphemeris'
  { -- | The AWS Ground Station ephemeris ID.
    ephemerisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEphemeris' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ephemerisId', 'describeEphemeris_ephemerisId' - The AWS Ground Station ephemeris ID.
newDescribeEphemeris ::
  -- | 'ephemerisId'
  Prelude.Text ->
  DescribeEphemeris
newDescribeEphemeris pEphemerisId_ =
  DescribeEphemeris' {ephemerisId = pEphemerisId_}

-- | The AWS Ground Station ephemeris ID.
describeEphemeris_ephemerisId :: Lens.Lens' DescribeEphemeris Prelude.Text
describeEphemeris_ephemerisId = Lens.lens (\DescribeEphemeris' {ephemerisId} -> ephemerisId) (\s@DescribeEphemeris' {} a -> s {ephemerisId = a} :: DescribeEphemeris)

instance Core.AWSRequest DescribeEphemeris where
  type
    AWSResponse DescribeEphemeris =
      DescribeEphemerisResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEphemerisResponse'
            Prelude.<$> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "enabled")
            Prelude.<*> (x Data..?> "ephemerisId")
            Prelude.<*> (x Data..?> "invalidReason")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "priority")
            Prelude.<*> (x Data..?> "satelliteId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "suppliedData")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEphemeris where
  hashWithSalt _salt DescribeEphemeris' {..} =
    _salt `Prelude.hashWithSalt` ephemerisId

instance Prelude.NFData DescribeEphemeris where
  rnf DescribeEphemeris' {..} = Prelude.rnf ephemerisId

instance Data.ToHeaders DescribeEphemeris where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeEphemeris where
  toPath DescribeEphemeris' {..} =
    Prelude.mconcat
      ["/ephemeris/", Data.toBS ephemerisId]

instance Data.ToQuery DescribeEphemeris where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEphemerisResponse' smart constructor.
data DescribeEphemerisResponse = DescribeEphemerisResponse'
  { -- | The time the ephemeris was uploaded in UTC.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Whether or not the ephemeris is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The AWS Ground Station ephemeris ID.
    ephemerisId :: Prelude.Maybe Prelude.Text,
    -- | Reason that an ephemeris failed validation. Only provided for
    -- ephemerides with @INVALID@ status.
    invalidReason :: Prelude.Maybe EphemerisInvalidReason,
    -- | A name string associated with the ephemeris. Used as a human-readable
    -- identifier for the ephemeris.
    name :: Prelude.Maybe Prelude.Text,
    -- | Customer-provided priority score to establish the order in which
    -- overlapping ephemerides should be used.
    --
    -- The default for customer-provided ephemeris priority is 1, and higher
    -- numbers take precedence.
    --
    -- Priority must be 1 or greater
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The AWS Ground Station satellite ID associated with ephemeris.
    satelliteId :: Prelude.Maybe Prelude.Text,
    -- | The status of the ephemeris.
    status :: Prelude.Maybe EphemerisStatus,
    -- | Supplied ephemeris data.
    suppliedData :: Prelude.Maybe EphemerisTypeDescription,
    -- | Tags assigned to an ephemeris.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEphemerisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeEphemerisResponse_creationTime' - The time the ephemeris was uploaded in UTC.
--
-- 'enabled', 'describeEphemerisResponse_enabled' - Whether or not the ephemeris is enabled.
--
-- 'ephemerisId', 'describeEphemerisResponse_ephemerisId' - The AWS Ground Station ephemeris ID.
--
-- 'invalidReason', 'describeEphemerisResponse_invalidReason' - Reason that an ephemeris failed validation. Only provided for
-- ephemerides with @INVALID@ status.
--
-- 'name', 'describeEphemerisResponse_name' - A name string associated with the ephemeris. Used as a human-readable
-- identifier for the ephemeris.
--
-- 'priority', 'describeEphemerisResponse_priority' - Customer-provided priority score to establish the order in which
-- overlapping ephemerides should be used.
--
-- The default for customer-provided ephemeris priority is 1, and higher
-- numbers take precedence.
--
-- Priority must be 1 or greater
--
-- 'satelliteId', 'describeEphemerisResponse_satelliteId' - The AWS Ground Station satellite ID associated with ephemeris.
--
-- 'status', 'describeEphemerisResponse_status' - The status of the ephemeris.
--
-- 'suppliedData', 'describeEphemerisResponse_suppliedData' - Supplied ephemeris data.
--
-- 'tags', 'describeEphemerisResponse_tags' - Tags assigned to an ephemeris.
--
-- 'httpStatus', 'describeEphemerisResponse_httpStatus' - The response's http status code.
newDescribeEphemerisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEphemerisResponse
newDescribeEphemerisResponse pHttpStatus_ =
  DescribeEphemerisResponse'
    { creationTime =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      ephemerisId = Prelude.Nothing,
      invalidReason = Prelude.Nothing,
      name = Prelude.Nothing,
      priority = Prelude.Nothing,
      satelliteId = Prelude.Nothing,
      status = Prelude.Nothing,
      suppliedData = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time the ephemeris was uploaded in UTC.
describeEphemerisResponse_creationTime :: Lens.Lens' DescribeEphemerisResponse (Prelude.Maybe Prelude.UTCTime)
describeEphemerisResponse_creationTime = Lens.lens (\DescribeEphemerisResponse' {creationTime} -> creationTime) (\s@DescribeEphemerisResponse' {} a -> s {creationTime = a} :: DescribeEphemerisResponse) Prelude.. Lens.mapping Data._Time

-- | Whether or not the ephemeris is enabled.
describeEphemerisResponse_enabled :: Lens.Lens' DescribeEphemerisResponse (Prelude.Maybe Prelude.Bool)
describeEphemerisResponse_enabled = Lens.lens (\DescribeEphemerisResponse' {enabled} -> enabled) (\s@DescribeEphemerisResponse' {} a -> s {enabled = a} :: DescribeEphemerisResponse)

-- | The AWS Ground Station ephemeris ID.
describeEphemerisResponse_ephemerisId :: Lens.Lens' DescribeEphemerisResponse (Prelude.Maybe Prelude.Text)
describeEphemerisResponse_ephemerisId = Lens.lens (\DescribeEphemerisResponse' {ephemerisId} -> ephemerisId) (\s@DescribeEphemerisResponse' {} a -> s {ephemerisId = a} :: DescribeEphemerisResponse)

-- | Reason that an ephemeris failed validation. Only provided for
-- ephemerides with @INVALID@ status.
describeEphemerisResponse_invalidReason :: Lens.Lens' DescribeEphemerisResponse (Prelude.Maybe EphemerisInvalidReason)
describeEphemerisResponse_invalidReason = Lens.lens (\DescribeEphemerisResponse' {invalidReason} -> invalidReason) (\s@DescribeEphemerisResponse' {} a -> s {invalidReason = a} :: DescribeEphemerisResponse)

-- | A name string associated with the ephemeris. Used as a human-readable
-- identifier for the ephemeris.
describeEphemerisResponse_name :: Lens.Lens' DescribeEphemerisResponse (Prelude.Maybe Prelude.Text)
describeEphemerisResponse_name = Lens.lens (\DescribeEphemerisResponse' {name} -> name) (\s@DescribeEphemerisResponse' {} a -> s {name = a} :: DescribeEphemerisResponse)

-- | Customer-provided priority score to establish the order in which
-- overlapping ephemerides should be used.
--
-- The default for customer-provided ephemeris priority is 1, and higher
-- numbers take precedence.
--
-- Priority must be 1 or greater
describeEphemerisResponse_priority :: Lens.Lens' DescribeEphemerisResponse (Prelude.Maybe Prelude.Natural)
describeEphemerisResponse_priority = Lens.lens (\DescribeEphemerisResponse' {priority} -> priority) (\s@DescribeEphemerisResponse' {} a -> s {priority = a} :: DescribeEphemerisResponse)

-- | The AWS Ground Station satellite ID associated with ephemeris.
describeEphemerisResponse_satelliteId :: Lens.Lens' DescribeEphemerisResponse (Prelude.Maybe Prelude.Text)
describeEphemerisResponse_satelliteId = Lens.lens (\DescribeEphemerisResponse' {satelliteId} -> satelliteId) (\s@DescribeEphemerisResponse' {} a -> s {satelliteId = a} :: DescribeEphemerisResponse)

-- | The status of the ephemeris.
describeEphemerisResponse_status :: Lens.Lens' DescribeEphemerisResponse (Prelude.Maybe EphemerisStatus)
describeEphemerisResponse_status = Lens.lens (\DescribeEphemerisResponse' {status} -> status) (\s@DescribeEphemerisResponse' {} a -> s {status = a} :: DescribeEphemerisResponse)

-- | Supplied ephemeris data.
describeEphemerisResponse_suppliedData :: Lens.Lens' DescribeEphemerisResponse (Prelude.Maybe EphemerisTypeDescription)
describeEphemerisResponse_suppliedData = Lens.lens (\DescribeEphemerisResponse' {suppliedData} -> suppliedData) (\s@DescribeEphemerisResponse' {} a -> s {suppliedData = a} :: DescribeEphemerisResponse)

-- | Tags assigned to an ephemeris.
describeEphemerisResponse_tags :: Lens.Lens' DescribeEphemerisResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeEphemerisResponse_tags = Lens.lens (\DescribeEphemerisResponse' {tags} -> tags) (\s@DescribeEphemerisResponse' {} a -> s {tags = a} :: DescribeEphemerisResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEphemerisResponse_httpStatus :: Lens.Lens' DescribeEphemerisResponse Prelude.Int
describeEphemerisResponse_httpStatus = Lens.lens (\DescribeEphemerisResponse' {httpStatus} -> httpStatus) (\s@DescribeEphemerisResponse' {} a -> s {httpStatus = a} :: DescribeEphemerisResponse)

instance Prelude.NFData DescribeEphemerisResponse where
  rnf DescribeEphemerisResponse' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf enabled `Prelude.seq`
        Prelude.rnf ephemerisId `Prelude.seq`
          Prelude.rnf invalidReason `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf priority `Prelude.seq`
                Prelude.rnf satelliteId `Prelude.seq`
                  Prelude.rnf status `Prelude.seq`
                    Prelude.rnf suppliedData `Prelude.seq`
                      Prelude.rnf tags `Prelude.seq`
                        Prelude.rnf httpStatus
