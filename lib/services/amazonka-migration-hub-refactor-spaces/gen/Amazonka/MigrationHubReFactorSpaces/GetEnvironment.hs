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
-- Module      : Amazonka.MigrationHubReFactorSpaces.GetEnvironment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Amazon Web Services Migration Hub Refactor Spaces environment.
module Amazonka.MigrationHubReFactorSpaces.GetEnvironment
  ( -- * Creating a Request
    GetEnvironment (..),
    newGetEnvironment,

    -- * Request Lenses
    getEnvironment_environmentIdentifier,

    -- * Destructuring the Response
    GetEnvironmentResponse (..),
    newGetEnvironmentResponse,

    -- * Response Lenses
    getEnvironmentResponse_tags,
    getEnvironmentResponse_name,
    getEnvironmentResponse_createdTime,
    getEnvironmentResponse_transitGatewayId,
    getEnvironmentResponse_arn,
    getEnvironmentResponse_state,
    getEnvironmentResponse_lastUpdatedTime,
    getEnvironmentResponse_description,
    getEnvironmentResponse_ownerAccountId,
    getEnvironmentResponse_environmentId,
    getEnvironmentResponse_error,
    getEnvironmentResponse_networkFabricType,
    getEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEnvironment' smart constructor.
data GetEnvironment = GetEnvironment'
  { -- | The ID of the environment.
    environmentIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentIdentifier', 'getEnvironment_environmentIdentifier' - The ID of the environment.
newGetEnvironment ::
  -- | 'environmentIdentifier'
  Prelude.Text ->
  GetEnvironment
newGetEnvironment pEnvironmentIdentifier_ =
  GetEnvironment'
    { environmentIdentifier =
        pEnvironmentIdentifier_
    }

-- | The ID of the environment.
getEnvironment_environmentIdentifier :: Lens.Lens' GetEnvironment Prelude.Text
getEnvironment_environmentIdentifier = Lens.lens (\GetEnvironment' {environmentIdentifier} -> environmentIdentifier) (\s@GetEnvironment' {} a -> s {environmentIdentifier = a} :: GetEnvironment)

instance Core.AWSRequest GetEnvironment where
  type
    AWSResponse GetEnvironment =
      GetEnvironmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEnvironmentResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "TransitGatewayId")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "LastUpdatedTime")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "OwnerAccountId")
            Prelude.<*> (x Data..?> "EnvironmentId")
            Prelude.<*> (x Data..?> "Error")
            Prelude.<*> (x Data..?> "NetworkFabricType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEnvironment where
  hashWithSalt _salt GetEnvironment' {..} =
    _salt `Prelude.hashWithSalt` environmentIdentifier

instance Prelude.NFData GetEnvironment where
  rnf GetEnvironment' {..} =
    Prelude.rnf environmentIdentifier

instance Data.ToHeaders GetEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEnvironment where
  toPath GetEnvironment' {..} =
    Prelude.mconcat
      ["/environments/", Data.toBS environmentIdentifier]

instance Data.ToQuery GetEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEnvironmentResponse' smart constructor.
data GetEnvironmentResponse = GetEnvironmentResponse'
  { -- | The tags to assign to the environment. A tag is a label that you assign
    -- to an Amazon Web Services resource. Each tag consists of a key-value
    -- pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The name of the environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the environment is created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the transit gateway set up by the environment.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the environment.
    state :: Prelude.Maybe EnvironmentState,
    -- | A timestamp that indicates when the environment was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the environment owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Any error associated with the environment resource.
    error :: Prelude.Maybe ErrorResponse,
    -- | The network fabric type of the environment.
    networkFabricType :: Prelude.Maybe NetworkFabricType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getEnvironmentResponse_tags' - The tags to assign to the environment. A tag is a label that you assign
-- to an Amazon Web Services resource. Each tag consists of a key-value
-- pair.
--
-- 'name', 'getEnvironmentResponse_name' - The name of the environment.
--
-- 'createdTime', 'getEnvironmentResponse_createdTime' - A timestamp that indicates when the environment is created.
--
-- 'transitGatewayId', 'getEnvironmentResponse_transitGatewayId' - The ID of the transit gateway set up by the environment.
--
-- 'arn', 'getEnvironmentResponse_arn' - The Amazon Resource Name (ARN) of the environment.
--
-- 'state', 'getEnvironmentResponse_state' - The current state of the environment.
--
-- 'lastUpdatedTime', 'getEnvironmentResponse_lastUpdatedTime' - A timestamp that indicates when the environment was last updated.
--
-- 'description', 'getEnvironmentResponse_description' - The description of the environment.
--
-- 'ownerAccountId', 'getEnvironmentResponse_ownerAccountId' - The Amazon Web Services account ID of the environment owner.
--
-- 'environmentId', 'getEnvironmentResponse_environmentId' - The unique identifier of the environment.
--
-- 'error', 'getEnvironmentResponse_error' - Any error associated with the environment resource.
--
-- 'networkFabricType', 'getEnvironmentResponse_networkFabricType' - The network fabric type of the environment.
--
-- 'httpStatus', 'getEnvironmentResponse_httpStatus' - The response's http status code.
newGetEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEnvironmentResponse
newGetEnvironmentResponse pHttpStatus_ =
  GetEnvironmentResponse'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      description = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      error = Prelude.Nothing,
      networkFabricType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags to assign to the environment. A tag is a label that you assign
-- to an Amazon Web Services resource. Each tag consists of a key-value
-- pair.
getEnvironmentResponse_tags :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getEnvironmentResponse_tags = Lens.lens (\GetEnvironmentResponse' {tags} -> tags) (\s@GetEnvironmentResponse' {} a -> s {tags = a} :: GetEnvironmentResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The name of the environment.
getEnvironmentResponse_name :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Text)
getEnvironmentResponse_name = Lens.lens (\GetEnvironmentResponse' {name} -> name) (\s@GetEnvironmentResponse' {} a -> s {name = a} :: GetEnvironmentResponse)

-- | A timestamp that indicates when the environment is created.
getEnvironmentResponse_createdTime :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.UTCTime)
getEnvironmentResponse_createdTime = Lens.lens (\GetEnvironmentResponse' {createdTime} -> createdTime) (\s@GetEnvironmentResponse' {} a -> s {createdTime = a} :: GetEnvironmentResponse) Prelude.. Lens.mapping Data._Time

-- | The ID of the transit gateway set up by the environment.
getEnvironmentResponse_transitGatewayId :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Text)
getEnvironmentResponse_transitGatewayId = Lens.lens (\GetEnvironmentResponse' {transitGatewayId} -> transitGatewayId) (\s@GetEnvironmentResponse' {} a -> s {transitGatewayId = a} :: GetEnvironmentResponse)

-- | The Amazon Resource Name (ARN) of the environment.
getEnvironmentResponse_arn :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Text)
getEnvironmentResponse_arn = Lens.lens (\GetEnvironmentResponse' {arn} -> arn) (\s@GetEnvironmentResponse' {} a -> s {arn = a} :: GetEnvironmentResponse)

-- | The current state of the environment.
getEnvironmentResponse_state :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe EnvironmentState)
getEnvironmentResponse_state = Lens.lens (\GetEnvironmentResponse' {state} -> state) (\s@GetEnvironmentResponse' {} a -> s {state = a} :: GetEnvironmentResponse)

-- | A timestamp that indicates when the environment was last updated.
getEnvironmentResponse_lastUpdatedTime :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.UTCTime)
getEnvironmentResponse_lastUpdatedTime = Lens.lens (\GetEnvironmentResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@GetEnvironmentResponse' {} a -> s {lastUpdatedTime = a} :: GetEnvironmentResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the environment.
getEnvironmentResponse_description :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Text)
getEnvironmentResponse_description = Lens.lens (\GetEnvironmentResponse' {description} -> description) (\s@GetEnvironmentResponse' {} a -> s {description = a} :: GetEnvironmentResponse)

-- | The Amazon Web Services account ID of the environment owner.
getEnvironmentResponse_ownerAccountId :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Text)
getEnvironmentResponse_ownerAccountId = Lens.lens (\GetEnvironmentResponse' {ownerAccountId} -> ownerAccountId) (\s@GetEnvironmentResponse' {} a -> s {ownerAccountId = a} :: GetEnvironmentResponse)

-- | The unique identifier of the environment.
getEnvironmentResponse_environmentId :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Text)
getEnvironmentResponse_environmentId = Lens.lens (\GetEnvironmentResponse' {environmentId} -> environmentId) (\s@GetEnvironmentResponse' {} a -> s {environmentId = a} :: GetEnvironmentResponse)

-- | Any error associated with the environment resource.
getEnvironmentResponse_error :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe ErrorResponse)
getEnvironmentResponse_error = Lens.lens (\GetEnvironmentResponse' {error} -> error) (\s@GetEnvironmentResponse' {} a -> s {error = a} :: GetEnvironmentResponse)

-- | The network fabric type of the environment.
getEnvironmentResponse_networkFabricType :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe NetworkFabricType)
getEnvironmentResponse_networkFabricType = Lens.lens (\GetEnvironmentResponse' {networkFabricType} -> networkFabricType) (\s@GetEnvironmentResponse' {} a -> s {networkFabricType = a} :: GetEnvironmentResponse)

-- | The response's http status code.
getEnvironmentResponse_httpStatus :: Lens.Lens' GetEnvironmentResponse Prelude.Int
getEnvironmentResponse_httpStatus = Lens.lens (\GetEnvironmentResponse' {httpStatus} -> httpStatus) (\s@GetEnvironmentResponse' {} a -> s {httpStatus = a} :: GetEnvironmentResponse)

instance Prelude.NFData GetEnvironmentResponse where
  rnf GetEnvironmentResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf networkFabricType
      `Prelude.seq` Prelude.rnf httpStatus
