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
-- Module      : Network.AWS.KafkaConnect.DescribeWorkerConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a worker configuration.
module Network.AWS.KafkaConnect.DescribeWorkerConfiguration
  ( -- * Creating a Request
    DescribeWorkerConfiguration (..),
    newDescribeWorkerConfiguration,

    -- * Request Lenses
    describeWorkerConfiguration_workerConfigurationArn,

    -- * Destructuring the Response
    DescribeWorkerConfigurationResponse (..),
    newDescribeWorkerConfigurationResponse,

    -- * Response Lenses
    describeWorkerConfigurationResponse_creationTime,
    describeWorkerConfigurationResponse_latestRevision,
    describeWorkerConfigurationResponse_name,
    describeWorkerConfigurationResponse_workerConfigurationArn,
    describeWorkerConfigurationResponse_description,
    describeWorkerConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KafkaConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeWorkerConfiguration' smart constructor.
data DescribeWorkerConfiguration = DescribeWorkerConfiguration'
  { -- | The Amazon Resource Name (ARN) of the worker configuration that you want
    -- to get information about.
    workerConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workerConfigurationArn', 'describeWorkerConfiguration_workerConfigurationArn' - The Amazon Resource Name (ARN) of the worker configuration that you want
-- to get information about.
newDescribeWorkerConfiguration ::
  -- | 'workerConfigurationArn'
  Prelude.Text ->
  DescribeWorkerConfiguration
newDescribeWorkerConfiguration
  pWorkerConfigurationArn_ =
    DescribeWorkerConfiguration'
      { workerConfigurationArn =
          pWorkerConfigurationArn_
      }

-- | The Amazon Resource Name (ARN) of the worker configuration that you want
-- to get information about.
describeWorkerConfiguration_workerConfigurationArn :: Lens.Lens' DescribeWorkerConfiguration Prelude.Text
describeWorkerConfiguration_workerConfigurationArn = Lens.lens (\DescribeWorkerConfiguration' {workerConfigurationArn} -> workerConfigurationArn) (\s@DescribeWorkerConfiguration' {} a -> s {workerConfigurationArn = a} :: DescribeWorkerConfiguration)

instance Core.AWSRequest DescribeWorkerConfiguration where
  type
    AWSResponse DescribeWorkerConfiguration =
      DescribeWorkerConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkerConfigurationResponse'
            Prelude.<$> (x Core..?> "creationTime")
            Prelude.<*> (x Core..?> "latestRevision")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "workerConfigurationArn")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWorkerConfiguration

instance Prelude.NFData DescribeWorkerConfiguration

instance Core.ToHeaders DescribeWorkerConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeWorkerConfiguration where
  toPath DescribeWorkerConfiguration' {..} =
    Prelude.mconcat
      [ "/v1/worker-configurations/",
        Core.toBS workerConfigurationArn
      ]

instance Core.ToQuery DescribeWorkerConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkerConfigurationResponse' smart constructor.
data DescribeWorkerConfigurationResponse = DescribeWorkerConfigurationResponse'
  { -- | The time that the worker configuration was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The latest revision of the custom configuration.
    latestRevision :: Prelude.Maybe WorkerConfigurationRevisionDescription,
    -- | The name of the worker configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the custom configuration.
    workerConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the worker configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkerConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeWorkerConfigurationResponse_creationTime' - The time that the worker configuration was created.
--
-- 'latestRevision', 'describeWorkerConfigurationResponse_latestRevision' - The latest revision of the custom configuration.
--
-- 'name', 'describeWorkerConfigurationResponse_name' - The name of the worker configuration.
--
-- 'workerConfigurationArn', 'describeWorkerConfigurationResponse_workerConfigurationArn' - The Amazon Resource Name (ARN) of the custom configuration.
--
-- 'description', 'describeWorkerConfigurationResponse_description' - The description of the worker configuration.
--
-- 'httpStatus', 'describeWorkerConfigurationResponse_httpStatus' - The response's http status code.
newDescribeWorkerConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWorkerConfigurationResponse
newDescribeWorkerConfigurationResponse pHttpStatus_ =
  DescribeWorkerConfigurationResponse'
    { creationTime =
        Prelude.Nothing,
      latestRevision = Prelude.Nothing,
      name = Prelude.Nothing,
      workerConfigurationArn =
        Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time that the worker configuration was created.
describeWorkerConfigurationResponse_creationTime :: Lens.Lens' DescribeWorkerConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
describeWorkerConfigurationResponse_creationTime = Lens.lens (\DescribeWorkerConfigurationResponse' {creationTime} -> creationTime) (\s@DescribeWorkerConfigurationResponse' {} a -> s {creationTime = a} :: DescribeWorkerConfigurationResponse) Prelude.. Lens.mapping Core._Time

-- | The latest revision of the custom configuration.
describeWorkerConfigurationResponse_latestRevision :: Lens.Lens' DescribeWorkerConfigurationResponse (Prelude.Maybe WorkerConfigurationRevisionDescription)
describeWorkerConfigurationResponse_latestRevision = Lens.lens (\DescribeWorkerConfigurationResponse' {latestRevision} -> latestRevision) (\s@DescribeWorkerConfigurationResponse' {} a -> s {latestRevision = a} :: DescribeWorkerConfigurationResponse)

-- | The name of the worker configuration.
describeWorkerConfigurationResponse_name :: Lens.Lens' DescribeWorkerConfigurationResponse (Prelude.Maybe Prelude.Text)
describeWorkerConfigurationResponse_name = Lens.lens (\DescribeWorkerConfigurationResponse' {name} -> name) (\s@DescribeWorkerConfigurationResponse' {} a -> s {name = a} :: DescribeWorkerConfigurationResponse)

-- | The Amazon Resource Name (ARN) of the custom configuration.
describeWorkerConfigurationResponse_workerConfigurationArn :: Lens.Lens' DescribeWorkerConfigurationResponse (Prelude.Maybe Prelude.Text)
describeWorkerConfigurationResponse_workerConfigurationArn = Lens.lens (\DescribeWorkerConfigurationResponse' {workerConfigurationArn} -> workerConfigurationArn) (\s@DescribeWorkerConfigurationResponse' {} a -> s {workerConfigurationArn = a} :: DescribeWorkerConfigurationResponse)

-- | The description of the worker configuration.
describeWorkerConfigurationResponse_description :: Lens.Lens' DescribeWorkerConfigurationResponse (Prelude.Maybe Prelude.Text)
describeWorkerConfigurationResponse_description = Lens.lens (\DescribeWorkerConfigurationResponse' {description} -> description) (\s@DescribeWorkerConfigurationResponse' {} a -> s {description = a} :: DescribeWorkerConfigurationResponse)

-- | The response's http status code.
describeWorkerConfigurationResponse_httpStatus :: Lens.Lens' DescribeWorkerConfigurationResponse Prelude.Int
describeWorkerConfigurationResponse_httpStatus = Lens.lens (\DescribeWorkerConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkerConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeWorkerConfigurationResponse)

instance
  Prelude.NFData
    DescribeWorkerConfigurationResponse
