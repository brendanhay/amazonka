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
-- Module      : Amazonka.Kafka.DescribeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of this MSK configuration.
module Amazonka.Kafka.DescribeConfiguration
  ( -- * Creating a Request
    DescribeConfiguration (..),
    newDescribeConfiguration,

    -- * Request Lenses
    describeConfiguration_arn,

    -- * Destructuring the Response
    DescribeConfigurationResponse (..),
    newDescribeConfigurationResponse,

    -- * Response Lenses
    describeConfigurationResponse_arn,
    describeConfigurationResponse_creationTime,
    describeConfigurationResponse_description,
    describeConfigurationResponse_kafkaVersions,
    describeConfigurationResponse_latestRevision,
    describeConfigurationResponse_name,
    describeConfigurationResponse_state,
    describeConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConfiguration' smart constructor.
data DescribeConfiguration = DescribeConfiguration'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies an MSK
    -- configuration and all of its revisions.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeConfiguration_arn' - The Amazon Resource Name (ARN) that uniquely identifies an MSK
-- configuration and all of its revisions.
newDescribeConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  DescribeConfiguration
newDescribeConfiguration pArn_ =
  DescribeConfiguration' {arn = pArn_}

-- | The Amazon Resource Name (ARN) that uniquely identifies an MSK
-- configuration and all of its revisions.
describeConfiguration_arn :: Lens.Lens' DescribeConfiguration Prelude.Text
describeConfiguration_arn = Lens.lens (\DescribeConfiguration' {arn} -> arn) (\s@DescribeConfiguration' {} a -> s {arn = a} :: DescribeConfiguration)

instance Core.AWSRequest DescribeConfiguration where
  type
    AWSResponse DescribeConfiguration =
      DescribeConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "kafkaVersions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "latestRevision")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConfiguration where
  hashWithSalt _salt DescribeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DescribeConfiguration where
  rnf DescribeConfiguration' {..} = Prelude.rnf arn

instance Data.ToHeaders DescribeConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeConfiguration where
  toPath DescribeConfiguration' {..} =
    Prelude.mconcat
      ["/v1/configurations/", Data.toBS arn]

instance Data.ToQuery DescribeConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConfigurationResponse' smart constructor.
data DescribeConfigurationResponse = DescribeConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) of the configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time when the configuration was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The description of the configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The versions of Apache Kafka with which you can use this MSK
    -- configuration.
    kafkaVersions :: Prelude.Maybe [Prelude.Text],
    -- | Latest revision of the configuration.
    latestRevision :: Prelude.Maybe ConfigurationRevision,
    -- | The name of the configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the configuration. The possible states are ACTIVE,
    -- DELETING, and DELETE_FAILED.
    state :: Prelude.Maybe ConfigurationState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeConfigurationResponse_arn' - The Amazon Resource Name (ARN) of the configuration.
--
-- 'creationTime', 'describeConfigurationResponse_creationTime' - The time when the configuration was created.
--
-- 'description', 'describeConfigurationResponse_description' - The description of the configuration.
--
-- 'kafkaVersions', 'describeConfigurationResponse_kafkaVersions' - The versions of Apache Kafka with which you can use this MSK
-- configuration.
--
-- 'latestRevision', 'describeConfigurationResponse_latestRevision' - Latest revision of the configuration.
--
-- 'name', 'describeConfigurationResponse_name' - The name of the configuration.
--
-- 'state', 'describeConfigurationResponse_state' - The state of the configuration. The possible states are ACTIVE,
-- DELETING, and DELETE_FAILED.
--
-- 'httpStatus', 'describeConfigurationResponse_httpStatus' - The response's http status code.
newDescribeConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigurationResponse
newDescribeConfigurationResponse pHttpStatus_ =
  DescribeConfigurationResponse'
    { arn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      kafkaVersions = Prelude.Nothing,
      latestRevision = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the configuration.
describeConfigurationResponse_arn :: Lens.Lens' DescribeConfigurationResponse (Prelude.Maybe Prelude.Text)
describeConfigurationResponse_arn = Lens.lens (\DescribeConfigurationResponse' {arn} -> arn) (\s@DescribeConfigurationResponse' {} a -> s {arn = a} :: DescribeConfigurationResponse)

-- | The time when the configuration was created.
describeConfigurationResponse_creationTime :: Lens.Lens' DescribeConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
describeConfigurationResponse_creationTime = Lens.lens (\DescribeConfigurationResponse' {creationTime} -> creationTime) (\s@DescribeConfigurationResponse' {} a -> s {creationTime = a} :: DescribeConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the configuration.
describeConfigurationResponse_description :: Lens.Lens' DescribeConfigurationResponse (Prelude.Maybe Prelude.Text)
describeConfigurationResponse_description = Lens.lens (\DescribeConfigurationResponse' {description} -> description) (\s@DescribeConfigurationResponse' {} a -> s {description = a} :: DescribeConfigurationResponse)

-- | The versions of Apache Kafka with which you can use this MSK
-- configuration.
describeConfigurationResponse_kafkaVersions :: Lens.Lens' DescribeConfigurationResponse (Prelude.Maybe [Prelude.Text])
describeConfigurationResponse_kafkaVersions = Lens.lens (\DescribeConfigurationResponse' {kafkaVersions} -> kafkaVersions) (\s@DescribeConfigurationResponse' {} a -> s {kafkaVersions = a} :: DescribeConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Latest revision of the configuration.
describeConfigurationResponse_latestRevision :: Lens.Lens' DescribeConfigurationResponse (Prelude.Maybe ConfigurationRevision)
describeConfigurationResponse_latestRevision = Lens.lens (\DescribeConfigurationResponse' {latestRevision} -> latestRevision) (\s@DescribeConfigurationResponse' {} a -> s {latestRevision = a} :: DescribeConfigurationResponse)

-- | The name of the configuration.
describeConfigurationResponse_name :: Lens.Lens' DescribeConfigurationResponse (Prelude.Maybe Prelude.Text)
describeConfigurationResponse_name = Lens.lens (\DescribeConfigurationResponse' {name} -> name) (\s@DescribeConfigurationResponse' {} a -> s {name = a} :: DescribeConfigurationResponse)

-- | The state of the configuration. The possible states are ACTIVE,
-- DELETING, and DELETE_FAILED.
describeConfigurationResponse_state :: Lens.Lens' DescribeConfigurationResponse (Prelude.Maybe ConfigurationState)
describeConfigurationResponse_state = Lens.lens (\DescribeConfigurationResponse' {state} -> state) (\s@DescribeConfigurationResponse' {} a -> s {state = a} :: DescribeConfigurationResponse)

-- | The response's http status code.
describeConfigurationResponse_httpStatus :: Lens.Lens' DescribeConfigurationResponse Prelude.Int
describeConfigurationResponse_httpStatus = Lens.lens (\DescribeConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationResponse)

instance Prelude.NFData DescribeConfigurationResponse where
  rnf DescribeConfigurationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kafkaVersions
      `Prelude.seq` Prelude.rnf latestRevision
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
