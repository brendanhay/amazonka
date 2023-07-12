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
-- Module      : Amazonka.KeySpaces.CreateKeyspace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateKeyspace@ operation adds a new keyspace to your account. In
-- an Amazon Web Services account, keyspace names must be unique within
-- each Region.
--
-- @CreateKeyspace@ is an asynchronous operation. You can monitor the
-- creation status of the new keyspace by using the @GetKeyspace@
-- operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/working-with-keyspaces.html#keyspaces-create Creating keyspaces>
-- in the /Amazon Keyspaces Developer Guide/.
module Amazonka.KeySpaces.CreateKeyspace
  ( -- * Creating a Request
    CreateKeyspace (..),
    newCreateKeyspace,

    -- * Request Lenses
    createKeyspace_tags,
    createKeyspace_keyspaceName,

    -- * Destructuring the Response
    CreateKeyspaceResponse (..),
    newCreateKeyspaceResponse,

    -- * Response Lenses
    createKeyspaceResponse_httpStatus,
    createKeyspaceResponse_resourceArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateKeyspace' smart constructor.
data CreateKeyspace = CreateKeyspace'
  { -- | A list of key-value pair tags to be attached to the keyspace.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/tagging-keyspaces.html Adding tags and labels to Amazon Keyspaces resources>
    -- in the /Amazon Keyspaces Developer Guide/.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the keyspace to be created.
    keyspaceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeyspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createKeyspace_tags' - A list of key-value pair tags to be attached to the keyspace.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/tagging-keyspaces.html Adding tags and labels to Amazon Keyspaces resources>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- 'keyspaceName', 'createKeyspace_keyspaceName' - The name of the keyspace to be created.
newCreateKeyspace ::
  -- | 'keyspaceName'
  Prelude.Text ->
  CreateKeyspace
newCreateKeyspace pKeyspaceName_ =
  CreateKeyspace'
    { tags = Prelude.Nothing,
      keyspaceName = pKeyspaceName_
    }

-- | A list of key-value pair tags to be attached to the keyspace.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/tagging-keyspaces.html Adding tags and labels to Amazon Keyspaces resources>
-- in the /Amazon Keyspaces Developer Guide/.
createKeyspace_tags :: Lens.Lens' CreateKeyspace (Prelude.Maybe (Prelude.NonEmpty Tag))
createKeyspace_tags = Lens.lens (\CreateKeyspace' {tags} -> tags) (\s@CreateKeyspace' {} a -> s {tags = a} :: CreateKeyspace) Prelude.. Lens.mapping Lens.coerced

-- | The name of the keyspace to be created.
createKeyspace_keyspaceName :: Lens.Lens' CreateKeyspace Prelude.Text
createKeyspace_keyspaceName = Lens.lens (\CreateKeyspace' {keyspaceName} -> keyspaceName) (\s@CreateKeyspace' {} a -> s {keyspaceName = a} :: CreateKeyspace)

instance Core.AWSRequest CreateKeyspace where
  type
    AWSResponse CreateKeyspace =
      CreateKeyspaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKeyspaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "resourceArn")
      )

instance Prelude.Hashable CreateKeyspace where
  hashWithSalt _salt CreateKeyspace' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` keyspaceName

instance Prelude.NFData CreateKeyspace where
  rnf CreateKeyspace' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf keyspaceName

instance Data.ToHeaders CreateKeyspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KeyspacesService.CreateKeyspace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateKeyspace where
  toJSON CreateKeyspace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("keyspaceName" Data..= keyspaceName)
          ]
      )

instance Data.ToPath CreateKeyspace where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateKeyspace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateKeyspaceResponse' smart constructor.
data CreateKeyspaceResponse = CreateKeyspaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of the keyspace in the format of an Amazon
    -- Resource Name (ARN).
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeyspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createKeyspaceResponse_httpStatus' - The response's http status code.
--
-- 'resourceArn', 'createKeyspaceResponse_resourceArn' - The unique identifier of the keyspace in the format of an Amazon
-- Resource Name (ARN).
newCreateKeyspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourceArn'
  Prelude.Text ->
  CreateKeyspaceResponse
newCreateKeyspaceResponse pHttpStatus_ pResourceArn_ =
  CreateKeyspaceResponse'
    { httpStatus = pHttpStatus_,
      resourceArn = pResourceArn_
    }

-- | The response's http status code.
createKeyspaceResponse_httpStatus :: Lens.Lens' CreateKeyspaceResponse Prelude.Int
createKeyspaceResponse_httpStatus = Lens.lens (\CreateKeyspaceResponse' {httpStatus} -> httpStatus) (\s@CreateKeyspaceResponse' {} a -> s {httpStatus = a} :: CreateKeyspaceResponse)

-- | The unique identifier of the keyspace in the format of an Amazon
-- Resource Name (ARN).
createKeyspaceResponse_resourceArn :: Lens.Lens' CreateKeyspaceResponse Prelude.Text
createKeyspaceResponse_resourceArn = Lens.lens (\CreateKeyspaceResponse' {resourceArn} -> resourceArn) (\s@CreateKeyspaceResponse' {} a -> s {resourceArn = a} :: CreateKeyspaceResponse)

instance Prelude.NFData CreateKeyspaceResponse where
  rnf CreateKeyspaceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceArn
