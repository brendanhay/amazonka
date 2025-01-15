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
-- Module      : Amazonka.Kafka.DescribeConfigurationRevision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of this revision of the configuration.
module Amazonka.Kafka.DescribeConfigurationRevision
  ( -- * Creating a Request
    DescribeConfigurationRevision (..),
    newDescribeConfigurationRevision,

    -- * Request Lenses
    describeConfigurationRevision_revision,
    describeConfigurationRevision_arn,

    -- * Destructuring the Response
    DescribeConfigurationRevisionResponse (..),
    newDescribeConfigurationRevisionResponse,

    -- * Response Lenses
    describeConfigurationRevisionResponse_arn,
    describeConfigurationRevisionResponse_creationTime,
    describeConfigurationRevisionResponse_description,
    describeConfigurationRevisionResponse_revision,
    describeConfigurationRevisionResponse_serverProperties,
    describeConfigurationRevisionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConfigurationRevision' smart constructor.
data DescribeConfigurationRevision = DescribeConfigurationRevision'
  { -- | A string that uniquely identifies a revision of an MSK configuration.
    revision :: Prelude.Integer,
    -- | The Amazon Resource Name (ARN) that uniquely identifies an MSK
    -- configuration and all of its revisions.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revision', 'describeConfigurationRevision_revision' - A string that uniquely identifies a revision of an MSK configuration.
--
-- 'arn', 'describeConfigurationRevision_arn' - The Amazon Resource Name (ARN) that uniquely identifies an MSK
-- configuration and all of its revisions.
newDescribeConfigurationRevision ::
  -- | 'revision'
  Prelude.Integer ->
  -- | 'arn'
  Prelude.Text ->
  DescribeConfigurationRevision
newDescribeConfigurationRevision pRevision_ pArn_ =
  DescribeConfigurationRevision'
    { revision =
        pRevision_,
      arn = pArn_
    }

-- | A string that uniquely identifies a revision of an MSK configuration.
describeConfigurationRevision_revision :: Lens.Lens' DescribeConfigurationRevision Prelude.Integer
describeConfigurationRevision_revision = Lens.lens (\DescribeConfigurationRevision' {revision} -> revision) (\s@DescribeConfigurationRevision' {} a -> s {revision = a} :: DescribeConfigurationRevision)

-- | The Amazon Resource Name (ARN) that uniquely identifies an MSK
-- configuration and all of its revisions.
describeConfigurationRevision_arn :: Lens.Lens' DescribeConfigurationRevision Prelude.Text
describeConfigurationRevision_arn = Lens.lens (\DescribeConfigurationRevision' {arn} -> arn) (\s@DescribeConfigurationRevision' {} a -> s {arn = a} :: DescribeConfigurationRevision)

instance
  Core.AWSRequest
    DescribeConfigurationRevision
  where
  type
    AWSResponse DescribeConfigurationRevision =
      DescribeConfigurationRevisionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationRevisionResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "revision")
            Prelude.<*> (x Data..?> "serverProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConfigurationRevision
  where
  hashWithSalt _salt DescribeConfigurationRevision' {..} =
    _salt
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DescribeConfigurationRevision where
  rnf DescribeConfigurationRevision' {..} =
    Prelude.rnf revision `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders DescribeConfigurationRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeConfigurationRevision where
  toPath DescribeConfigurationRevision' {..} =
    Prelude.mconcat
      [ "/v1/configurations/",
        Data.toBS arn,
        "/revisions/",
        Data.toBS revision
      ]

instance Data.ToQuery DescribeConfigurationRevision where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConfigurationRevisionResponse' smart constructor.
data DescribeConfigurationRevisionResponse = DescribeConfigurationRevisionResponse'
  { -- | The Amazon Resource Name (ARN) of the configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time when the configuration was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The description of the configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The revision number.
    revision :: Prelude.Maybe Prelude.Integer,
    -- | Contents of the server.properties file. When using the API, you must
    -- ensure that the contents of the file are base64 encoded. When using the
    -- AWS Management Console, the SDK, or the AWS CLI, the contents of
    -- server.properties can be in plaintext.
    serverProperties :: Prelude.Maybe Data.Base64,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeConfigurationRevisionResponse_arn' - The Amazon Resource Name (ARN) of the configuration.
--
-- 'creationTime', 'describeConfigurationRevisionResponse_creationTime' - The time when the configuration was created.
--
-- 'description', 'describeConfigurationRevisionResponse_description' - The description of the configuration.
--
-- 'revision', 'describeConfigurationRevisionResponse_revision' - The revision number.
--
-- 'serverProperties', 'describeConfigurationRevisionResponse_serverProperties' - Contents of the server.properties file. When using the API, you must
-- ensure that the contents of the file are base64 encoded. When using the
-- AWS Management Console, the SDK, or the AWS CLI, the contents of
-- server.properties can be in plaintext.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'httpStatus', 'describeConfigurationRevisionResponse_httpStatus' - The response's http status code.
newDescribeConfigurationRevisionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigurationRevisionResponse
newDescribeConfigurationRevisionResponse pHttpStatus_ =
  DescribeConfigurationRevisionResponse'
    { arn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      revision = Prelude.Nothing,
      serverProperties = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the configuration.
describeConfigurationRevisionResponse_arn :: Lens.Lens' DescribeConfigurationRevisionResponse (Prelude.Maybe Prelude.Text)
describeConfigurationRevisionResponse_arn = Lens.lens (\DescribeConfigurationRevisionResponse' {arn} -> arn) (\s@DescribeConfigurationRevisionResponse' {} a -> s {arn = a} :: DescribeConfigurationRevisionResponse)

-- | The time when the configuration was created.
describeConfigurationRevisionResponse_creationTime :: Lens.Lens' DescribeConfigurationRevisionResponse (Prelude.Maybe Prelude.UTCTime)
describeConfigurationRevisionResponse_creationTime = Lens.lens (\DescribeConfigurationRevisionResponse' {creationTime} -> creationTime) (\s@DescribeConfigurationRevisionResponse' {} a -> s {creationTime = a} :: DescribeConfigurationRevisionResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the configuration.
describeConfigurationRevisionResponse_description :: Lens.Lens' DescribeConfigurationRevisionResponse (Prelude.Maybe Prelude.Text)
describeConfigurationRevisionResponse_description = Lens.lens (\DescribeConfigurationRevisionResponse' {description} -> description) (\s@DescribeConfigurationRevisionResponse' {} a -> s {description = a} :: DescribeConfigurationRevisionResponse)

-- | The revision number.
describeConfigurationRevisionResponse_revision :: Lens.Lens' DescribeConfigurationRevisionResponse (Prelude.Maybe Prelude.Integer)
describeConfigurationRevisionResponse_revision = Lens.lens (\DescribeConfigurationRevisionResponse' {revision} -> revision) (\s@DescribeConfigurationRevisionResponse' {} a -> s {revision = a} :: DescribeConfigurationRevisionResponse)

-- | Contents of the server.properties file. When using the API, you must
-- ensure that the contents of the file are base64 encoded. When using the
-- AWS Management Console, the SDK, or the AWS CLI, the contents of
-- server.properties can be in plaintext.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
describeConfigurationRevisionResponse_serverProperties :: Lens.Lens' DescribeConfigurationRevisionResponse (Prelude.Maybe Prelude.ByteString)
describeConfigurationRevisionResponse_serverProperties = Lens.lens (\DescribeConfigurationRevisionResponse' {serverProperties} -> serverProperties) (\s@DescribeConfigurationRevisionResponse' {} a -> s {serverProperties = a} :: DescribeConfigurationRevisionResponse) Prelude.. Lens.mapping Data._Base64

-- | The response's http status code.
describeConfigurationRevisionResponse_httpStatus :: Lens.Lens' DescribeConfigurationRevisionResponse Prelude.Int
describeConfigurationRevisionResponse_httpStatus = Lens.lens (\DescribeConfigurationRevisionResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationRevisionResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationRevisionResponse)

instance
  Prelude.NFData
    DescribeConfigurationRevisionResponse
  where
  rnf DescribeConfigurationRevisionResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf revision `Prelude.seq`
            Prelude.rnf serverProperties `Prelude.seq`
              Prelude.rnf httpStatus
