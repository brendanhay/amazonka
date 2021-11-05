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
-- Module      : Amazonka.RobOMaker.DescribeFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a fleet.
module Amazonka.RobOMaker.DescribeFleet
  ( -- * Creating a Request
    DescribeFleet (..),
    newDescribeFleet,

    -- * Request Lenses
    describeFleet_fleet,

    -- * Destructuring the Response
    DescribeFleetResponse (..),
    newDescribeFleetResponse,

    -- * Response Lenses
    describeFleetResponse_lastDeploymentJob,
    describeFleetResponse_lastDeploymentStatus,
    describeFleetResponse_robots,
    describeFleetResponse_arn,
    describeFleetResponse_createdAt,
    describeFleetResponse_name,
    describeFleetResponse_lastDeploymentTime,
    describeFleetResponse_tags,
    describeFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newDescribeFleet' smart constructor.
data DescribeFleet = DescribeFleet'
  { -- | The Amazon Resource Name (ARN) of the fleet.
    fleet :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleet', 'describeFleet_fleet' - The Amazon Resource Name (ARN) of the fleet.
newDescribeFleet ::
  -- | 'fleet'
  Prelude.Text ->
  DescribeFleet
newDescribeFleet pFleet_ =
  DescribeFleet' {fleet = pFleet_}

-- | The Amazon Resource Name (ARN) of the fleet.
describeFleet_fleet :: Lens.Lens' DescribeFleet Prelude.Text
describeFleet_fleet = Lens.lens (\DescribeFleet' {fleet} -> fleet) (\s@DescribeFleet' {} a -> s {fleet = a} :: DescribeFleet)

instance Core.AWSRequest DescribeFleet where
  type
    AWSResponse DescribeFleet =
      DescribeFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetResponse'
            Prelude.<$> (x Core..?> "lastDeploymentJob")
            Prelude.<*> (x Core..?> "lastDeploymentStatus")
            Prelude.<*> (x Core..?> "robots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "lastDeploymentTime")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleet

instance Prelude.NFData DescribeFleet

instance Core.ToHeaders DescribeFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFleet where
  toJSON DescribeFleet' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("fleet" Core..= fleet)]
      )

instance Core.ToPath DescribeFleet where
  toPath = Prelude.const "/describeFleet"

instance Core.ToQuery DescribeFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFleetResponse' smart constructor.
data DescribeFleetResponse = DescribeFleetResponse'
  { -- | The Amazon Resource Name (ARN) of the last deployment job.
    lastDeploymentJob :: Prelude.Maybe Prelude.Text,
    -- | The status of the last deployment.
    lastDeploymentStatus :: Prelude.Maybe DeploymentStatus,
    -- | A list of robots.
    robots :: Prelude.Maybe [Robot],
    -- | The Amazon Resource Name (ARN) of the fleet.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the fleet was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The name of the fleet.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time of the last deployment.
    lastDeploymentTime :: Prelude.Maybe Core.POSIX,
    -- | The list of all tags added to the specified fleet.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastDeploymentJob', 'describeFleetResponse_lastDeploymentJob' - The Amazon Resource Name (ARN) of the last deployment job.
--
-- 'lastDeploymentStatus', 'describeFleetResponse_lastDeploymentStatus' - The status of the last deployment.
--
-- 'robots', 'describeFleetResponse_robots' - A list of robots.
--
-- 'arn', 'describeFleetResponse_arn' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'createdAt', 'describeFleetResponse_createdAt' - The time, in milliseconds since the epoch, when the fleet was created.
--
-- 'name', 'describeFleetResponse_name' - The name of the fleet.
--
-- 'lastDeploymentTime', 'describeFleetResponse_lastDeploymentTime' - The time of the last deployment.
--
-- 'tags', 'describeFleetResponse_tags' - The list of all tags added to the specified fleet.
--
-- 'httpStatus', 'describeFleetResponse_httpStatus' - The response's http status code.
newDescribeFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetResponse
newDescribeFleetResponse pHttpStatus_ =
  DescribeFleetResponse'
    { lastDeploymentJob =
        Prelude.Nothing,
      lastDeploymentStatus = Prelude.Nothing,
      robots = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      name = Prelude.Nothing,
      lastDeploymentTime = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the last deployment job.
describeFleetResponse_lastDeploymentJob :: Lens.Lens' DescribeFleetResponse (Prelude.Maybe Prelude.Text)
describeFleetResponse_lastDeploymentJob = Lens.lens (\DescribeFleetResponse' {lastDeploymentJob} -> lastDeploymentJob) (\s@DescribeFleetResponse' {} a -> s {lastDeploymentJob = a} :: DescribeFleetResponse)

-- | The status of the last deployment.
describeFleetResponse_lastDeploymentStatus :: Lens.Lens' DescribeFleetResponse (Prelude.Maybe DeploymentStatus)
describeFleetResponse_lastDeploymentStatus = Lens.lens (\DescribeFleetResponse' {lastDeploymentStatus} -> lastDeploymentStatus) (\s@DescribeFleetResponse' {} a -> s {lastDeploymentStatus = a} :: DescribeFleetResponse)

-- | A list of robots.
describeFleetResponse_robots :: Lens.Lens' DescribeFleetResponse (Prelude.Maybe [Robot])
describeFleetResponse_robots = Lens.lens (\DescribeFleetResponse' {robots} -> robots) (\s@DescribeFleetResponse' {} a -> s {robots = a} :: DescribeFleetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the fleet.
describeFleetResponse_arn :: Lens.Lens' DescribeFleetResponse (Prelude.Maybe Prelude.Text)
describeFleetResponse_arn = Lens.lens (\DescribeFleetResponse' {arn} -> arn) (\s@DescribeFleetResponse' {} a -> s {arn = a} :: DescribeFleetResponse)

-- | The time, in milliseconds since the epoch, when the fleet was created.
describeFleetResponse_createdAt :: Lens.Lens' DescribeFleetResponse (Prelude.Maybe Prelude.UTCTime)
describeFleetResponse_createdAt = Lens.lens (\DescribeFleetResponse' {createdAt} -> createdAt) (\s@DescribeFleetResponse' {} a -> s {createdAt = a} :: DescribeFleetResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the fleet.
describeFleetResponse_name :: Lens.Lens' DescribeFleetResponse (Prelude.Maybe Prelude.Text)
describeFleetResponse_name = Lens.lens (\DescribeFleetResponse' {name} -> name) (\s@DescribeFleetResponse' {} a -> s {name = a} :: DescribeFleetResponse)

-- | The time of the last deployment.
describeFleetResponse_lastDeploymentTime :: Lens.Lens' DescribeFleetResponse (Prelude.Maybe Prelude.UTCTime)
describeFleetResponse_lastDeploymentTime = Lens.lens (\DescribeFleetResponse' {lastDeploymentTime} -> lastDeploymentTime) (\s@DescribeFleetResponse' {} a -> s {lastDeploymentTime = a} :: DescribeFleetResponse) Prelude.. Lens.mapping Core._Time

-- | The list of all tags added to the specified fleet.
describeFleetResponse_tags :: Lens.Lens' DescribeFleetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeFleetResponse_tags = Lens.lens (\DescribeFleetResponse' {tags} -> tags) (\s@DescribeFleetResponse' {} a -> s {tags = a} :: DescribeFleetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeFleetResponse_httpStatus :: Lens.Lens' DescribeFleetResponse Prelude.Int
describeFleetResponse_httpStatus = Lens.lens (\DescribeFleetResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetResponse' {} a -> s {httpStatus = a} :: DescribeFleetResponse)

instance Prelude.NFData DescribeFleetResponse
