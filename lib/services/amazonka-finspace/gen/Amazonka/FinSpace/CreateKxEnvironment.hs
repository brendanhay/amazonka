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
-- Module      : Amazonka.FinSpace.CreateKxEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a managed kdb environment for the account.
module Amazonka.FinSpace.CreateKxEnvironment
  ( -- * Creating a Request
    CreateKxEnvironment (..),
    newCreateKxEnvironment,

    -- * Request Lenses
    createKxEnvironment_clientToken,
    createKxEnvironment_description,
    createKxEnvironment_tags,
    createKxEnvironment_name,
    createKxEnvironment_kmsKeyId,

    -- * Destructuring the Response
    CreateKxEnvironmentResponse (..),
    newCreateKxEnvironmentResponse,

    -- * Response Lenses
    createKxEnvironmentResponse_creationTimestamp,
    createKxEnvironmentResponse_description,
    createKxEnvironmentResponse_environmentArn,
    createKxEnvironmentResponse_environmentId,
    createKxEnvironmentResponse_kmsKeyId,
    createKxEnvironmentResponse_name,
    createKxEnvironmentResponse_status,
    createKxEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateKxEnvironment' smart constructor.
data CreateKxEnvironment = CreateKxEnvironment'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the kdb environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs to label the kdb environment. You can add up
    -- to 50 tags to your kdb environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the kdb environment that you want to create.
    name :: Prelude.Text,
    -- | The KMS key ID to encrypt your data in the FinSpace environment.
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKxEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createKxEnvironment_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'description', 'createKxEnvironment_description' - A description for the kdb environment.
--
-- 'tags', 'createKxEnvironment_tags' - A list of key-value pairs to label the kdb environment. You can add up
-- to 50 tags to your kdb environment.
--
-- 'name', 'createKxEnvironment_name' - The name of the kdb environment that you want to create.
--
-- 'kmsKeyId', 'createKxEnvironment_kmsKeyId' - The KMS key ID to encrypt your data in the FinSpace environment.
newCreateKxEnvironment ::
  -- | 'name'
  Prelude.Text ->
  -- | 'kmsKeyId'
  Prelude.Text ->
  CreateKxEnvironment
newCreateKxEnvironment pName_ pKmsKeyId_ =
  CreateKxEnvironment'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      kmsKeyId = pKmsKeyId_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
createKxEnvironment_clientToken :: Lens.Lens' CreateKxEnvironment (Prelude.Maybe Prelude.Text)
createKxEnvironment_clientToken = Lens.lens (\CreateKxEnvironment' {clientToken} -> clientToken) (\s@CreateKxEnvironment' {} a -> s {clientToken = a} :: CreateKxEnvironment)

-- | A description for the kdb environment.
createKxEnvironment_description :: Lens.Lens' CreateKxEnvironment (Prelude.Maybe Prelude.Text)
createKxEnvironment_description = Lens.lens (\CreateKxEnvironment' {description} -> description) (\s@CreateKxEnvironment' {} a -> s {description = a} :: CreateKxEnvironment)

-- | A list of key-value pairs to label the kdb environment. You can add up
-- to 50 tags to your kdb environment.
createKxEnvironment_tags :: Lens.Lens' CreateKxEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createKxEnvironment_tags = Lens.lens (\CreateKxEnvironment' {tags} -> tags) (\s@CreateKxEnvironment' {} a -> s {tags = a} :: CreateKxEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The name of the kdb environment that you want to create.
createKxEnvironment_name :: Lens.Lens' CreateKxEnvironment Prelude.Text
createKxEnvironment_name = Lens.lens (\CreateKxEnvironment' {name} -> name) (\s@CreateKxEnvironment' {} a -> s {name = a} :: CreateKxEnvironment)

-- | The KMS key ID to encrypt your data in the FinSpace environment.
createKxEnvironment_kmsKeyId :: Lens.Lens' CreateKxEnvironment Prelude.Text
createKxEnvironment_kmsKeyId = Lens.lens (\CreateKxEnvironment' {kmsKeyId} -> kmsKeyId) (\s@CreateKxEnvironment' {} a -> s {kmsKeyId = a} :: CreateKxEnvironment)

instance Core.AWSRequest CreateKxEnvironment where
  type
    AWSResponse CreateKxEnvironment =
      CreateKxEnvironmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKxEnvironmentResponse'
            Prelude.<$> (x Data..?> "creationTimestamp")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "environmentArn")
            Prelude.<*> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "kmsKeyId")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateKxEnvironment where
  hashWithSalt _salt CreateKxEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData CreateKxEnvironment where
  rnf CreateKxEnvironment' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Data.ToHeaders CreateKxEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateKxEnvironment where
  toJSON CreateKxEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("kmsKeyId" Data..= kmsKeyId)
          ]
      )

instance Data.ToPath CreateKxEnvironment where
  toPath = Prelude.const "/kx/environments"

instance Data.ToQuery CreateKxEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateKxEnvironmentResponse' smart constructor.
data CreateKxEnvironmentResponse = CreateKxEnvironmentResponse'
  { -- | The timestamp at which the kdb environment was created in FinSpace.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A description for the kdb environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN identifier of the environment.
    environmentArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The KMS key ID to encrypt your data in the FinSpace environment.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the kdb environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the kdb environment.
    status :: Prelude.Maybe EnvironmentStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKxEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createKxEnvironmentResponse_creationTimestamp' - The timestamp at which the kdb environment was created in FinSpace.
--
-- 'description', 'createKxEnvironmentResponse_description' - A description for the kdb environment.
--
-- 'environmentArn', 'createKxEnvironmentResponse_environmentArn' - The ARN identifier of the environment.
--
-- 'environmentId', 'createKxEnvironmentResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'kmsKeyId', 'createKxEnvironmentResponse_kmsKeyId' - The KMS key ID to encrypt your data in the FinSpace environment.
--
-- 'name', 'createKxEnvironmentResponse_name' - The name of the kdb environment.
--
-- 'status', 'createKxEnvironmentResponse_status' - The status of the kdb environment.
--
-- 'httpStatus', 'createKxEnvironmentResponse_httpStatus' - The response's http status code.
newCreateKxEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateKxEnvironmentResponse
newCreateKxEnvironmentResponse pHttpStatus_ =
  CreateKxEnvironmentResponse'
    { creationTimestamp =
        Prelude.Nothing,
      description = Prelude.Nothing,
      environmentArn = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp at which the kdb environment was created in FinSpace.
createKxEnvironmentResponse_creationTimestamp :: Lens.Lens' CreateKxEnvironmentResponse (Prelude.Maybe Prelude.UTCTime)
createKxEnvironmentResponse_creationTimestamp = Lens.lens (\CreateKxEnvironmentResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateKxEnvironmentResponse' {} a -> s {creationTimestamp = a} :: CreateKxEnvironmentResponse) Prelude.. Lens.mapping Data._Time

-- | A description for the kdb environment.
createKxEnvironmentResponse_description :: Lens.Lens' CreateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
createKxEnvironmentResponse_description = Lens.lens (\CreateKxEnvironmentResponse' {description} -> description) (\s@CreateKxEnvironmentResponse' {} a -> s {description = a} :: CreateKxEnvironmentResponse)

-- | The ARN identifier of the environment.
createKxEnvironmentResponse_environmentArn :: Lens.Lens' CreateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
createKxEnvironmentResponse_environmentArn = Lens.lens (\CreateKxEnvironmentResponse' {environmentArn} -> environmentArn) (\s@CreateKxEnvironmentResponse' {} a -> s {environmentArn = a} :: CreateKxEnvironmentResponse)

-- | A unique identifier for the kdb environment.
createKxEnvironmentResponse_environmentId :: Lens.Lens' CreateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
createKxEnvironmentResponse_environmentId = Lens.lens (\CreateKxEnvironmentResponse' {environmentId} -> environmentId) (\s@CreateKxEnvironmentResponse' {} a -> s {environmentId = a} :: CreateKxEnvironmentResponse)

-- | The KMS key ID to encrypt your data in the FinSpace environment.
createKxEnvironmentResponse_kmsKeyId :: Lens.Lens' CreateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
createKxEnvironmentResponse_kmsKeyId = Lens.lens (\CreateKxEnvironmentResponse' {kmsKeyId} -> kmsKeyId) (\s@CreateKxEnvironmentResponse' {} a -> s {kmsKeyId = a} :: CreateKxEnvironmentResponse)

-- | The name of the kdb environment.
createKxEnvironmentResponse_name :: Lens.Lens' CreateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
createKxEnvironmentResponse_name = Lens.lens (\CreateKxEnvironmentResponse' {name} -> name) (\s@CreateKxEnvironmentResponse' {} a -> s {name = a} :: CreateKxEnvironmentResponse)

-- | The status of the kdb environment.
createKxEnvironmentResponse_status :: Lens.Lens' CreateKxEnvironmentResponse (Prelude.Maybe EnvironmentStatus)
createKxEnvironmentResponse_status = Lens.lens (\CreateKxEnvironmentResponse' {status} -> status) (\s@CreateKxEnvironmentResponse' {} a -> s {status = a} :: CreateKxEnvironmentResponse)

-- | The response's http status code.
createKxEnvironmentResponse_httpStatus :: Lens.Lens' CreateKxEnvironmentResponse Prelude.Int
createKxEnvironmentResponse_httpStatus = Lens.lens (\CreateKxEnvironmentResponse' {httpStatus} -> httpStatus) (\s@CreateKxEnvironmentResponse' {} a -> s {httpStatus = a} :: CreateKxEnvironmentResponse)

instance Prelude.NFData CreateKxEnvironmentResponse where
  rnf CreateKxEnvironmentResponse' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentArn
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
