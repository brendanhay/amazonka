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
-- Module      : Amazonka.QuickSight.CreateNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- (Enterprise edition only) Creates a new namespace for you to use with
-- Amazon QuickSight.
--
-- A namespace allows you to isolate the Amazon QuickSight users and groups
-- that are registered for that namespace. Users that access the namespace
-- can share assets only with other users or groups in the same namespace.
-- They can\'t see users and groups in other namespaces. You can create a
-- namespace after your Amazon Web Services account is subscribed to Amazon
-- QuickSight. The namespace must be unique within the Amazon Web Services
-- account. By default, there is a limit of 100 namespaces per Amazon Web
-- Services account. To increase your limit, create a ticket with Amazon
-- Web Services Support.
module Amazonka.QuickSight.CreateNamespace
  ( -- * Creating a Request
    CreateNamespace (..),
    newCreateNamespace,

    -- * Request Lenses
    createNamespace_tags,
    createNamespace_awsAccountId,
    createNamespace_namespace,
    createNamespace_identityStore,

    -- * Destructuring the Response
    CreateNamespaceResponse (..),
    newCreateNamespaceResponse,

    -- * Response Lenses
    createNamespaceResponse_arn,
    createNamespaceResponse_capacityRegion,
    createNamespaceResponse_creationStatus,
    createNamespaceResponse_identityStore,
    createNamespaceResponse_name,
    createNamespaceResponse_requestId,
    createNamespaceResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNamespace' smart constructor.
data CreateNamespace = CreateNamespace'
  { -- | The tags that you want to associate with the namespace that you\'re
    -- creating.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ID for the Amazon Web Services account that you want to create the
    -- Amazon QuickSight namespace in.
    awsAccountId :: Prelude.Text,
    -- | The name that you want to use to describe the new namespace.
    namespace :: Prelude.Text,
    -- | Specifies the type of your user identity directory. Currently, this
    -- supports users with an identity type of @QUICKSIGHT@.
    identityStore :: IdentityStore
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createNamespace_tags' - The tags that you want to associate with the namespace that you\'re
-- creating.
--
-- 'awsAccountId', 'createNamespace_awsAccountId' - The ID for the Amazon Web Services account that you want to create the
-- Amazon QuickSight namespace in.
--
-- 'namespace', 'createNamespace_namespace' - The name that you want to use to describe the new namespace.
--
-- 'identityStore', 'createNamespace_identityStore' - Specifies the type of your user identity directory. Currently, this
-- supports users with an identity type of @QUICKSIGHT@.
newCreateNamespace ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  -- | 'identityStore'
  IdentityStore ->
  CreateNamespace
newCreateNamespace
  pAwsAccountId_
  pNamespace_
  pIdentityStore_ =
    CreateNamespace'
      { tags = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        namespace = pNamespace_,
        identityStore = pIdentityStore_
      }

-- | The tags that you want to associate with the namespace that you\'re
-- creating.
createNamespace_tags :: Lens.Lens' CreateNamespace (Prelude.Maybe (Prelude.NonEmpty Tag))
createNamespace_tags = Lens.lens (\CreateNamespace' {tags} -> tags) (\s@CreateNamespace' {} a -> s {tags = a} :: CreateNamespace) Prelude.. Lens.mapping Lens.coerced

-- | The ID for the Amazon Web Services account that you want to create the
-- Amazon QuickSight namespace in.
createNamespace_awsAccountId :: Lens.Lens' CreateNamespace Prelude.Text
createNamespace_awsAccountId = Lens.lens (\CreateNamespace' {awsAccountId} -> awsAccountId) (\s@CreateNamespace' {} a -> s {awsAccountId = a} :: CreateNamespace)

-- | The name that you want to use to describe the new namespace.
createNamespace_namespace :: Lens.Lens' CreateNamespace Prelude.Text
createNamespace_namespace = Lens.lens (\CreateNamespace' {namespace} -> namespace) (\s@CreateNamespace' {} a -> s {namespace = a} :: CreateNamespace)

-- | Specifies the type of your user identity directory. Currently, this
-- supports users with an identity type of @QUICKSIGHT@.
createNamespace_identityStore :: Lens.Lens' CreateNamespace IdentityStore
createNamespace_identityStore = Lens.lens (\CreateNamespace' {identityStore} -> identityStore) (\s@CreateNamespace' {} a -> s {identityStore = a} :: CreateNamespace)

instance Core.AWSRequest CreateNamespace where
  type
    AWSResponse CreateNamespace =
      CreateNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNamespaceResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CapacityRegion")
            Prelude.<*> (x Data..?> "CreationStatus")
            Prelude.<*> (x Data..?> "IdentityStore")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNamespace where
  hashWithSalt _salt CreateNamespace' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` identityStore

instance Prelude.NFData CreateNamespace where
  rnf CreateNamespace' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf identityStore

instance Data.ToHeaders CreateNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateNamespace where
  toJSON CreateNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Namespace" Data..= namespace),
            Prelude.Just
              ("IdentityStore" Data..= identityStore)
          ]
      )

instance Data.ToPath CreateNamespace where
  toPath CreateNamespace' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS awsAccountId]

instance Data.ToQuery CreateNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNamespaceResponse' smart constructor.
data CreateNamespaceResponse = CreateNamespaceResponse'
  { -- | The ARN of the Amazon QuickSight namespace you created.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region; that you want to use for the free SPICE
    -- capacity for the new namespace. This is set to the region that you run
    -- CreateNamespace in.
    capacityRegion :: Prelude.Maybe Prelude.Text,
    -- | The status of the creation of the namespace. This is an asynchronous
    -- process. A status of @CREATED@ means that your namespace is ready to
    -- use. If an error occurs, it indicates if the process is @retryable@ or
    -- @non-retryable@. In the case of a non-retryable error, refer to the
    -- error message for follow-up tasks.
    creationStatus :: Prelude.Maybe NamespaceStatus,
    -- | Specifies the type of your user identity directory. Currently, this
    -- supports users with an identity type of @QUICKSIGHT@.
    identityStore :: Prelude.Maybe IdentityStore,
    -- | The name of the new namespace that you created.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createNamespaceResponse_arn' - The ARN of the Amazon QuickSight namespace you created.
--
-- 'capacityRegion', 'createNamespaceResponse_capacityRegion' - The Amazon Web Services Region; that you want to use for the free SPICE
-- capacity for the new namespace. This is set to the region that you run
-- CreateNamespace in.
--
-- 'creationStatus', 'createNamespaceResponse_creationStatus' - The status of the creation of the namespace. This is an asynchronous
-- process. A status of @CREATED@ means that your namespace is ready to
-- use. If an error occurs, it indicates if the process is @retryable@ or
-- @non-retryable@. In the case of a non-retryable error, refer to the
-- error message for follow-up tasks.
--
-- 'identityStore', 'createNamespaceResponse_identityStore' - Specifies the type of your user identity directory. Currently, this
-- supports users with an identity type of @QUICKSIGHT@.
--
-- 'name', 'createNamespaceResponse_name' - The name of the new namespace that you created.
--
-- 'requestId', 'createNamespaceResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'createNamespaceResponse_status' - The HTTP status of the request.
newCreateNamespaceResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateNamespaceResponse
newCreateNamespaceResponse pStatus_ =
  CreateNamespaceResponse'
    { arn = Prelude.Nothing,
      capacityRegion = Prelude.Nothing,
      creationStatus = Prelude.Nothing,
      identityStore = Prelude.Nothing,
      name = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The ARN of the Amazon QuickSight namespace you created.
createNamespaceResponse_arn :: Lens.Lens' CreateNamespaceResponse (Prelude.Maybe Prelude.Text)
createNamespaceResponse_arn = Lens.lens (\CreateNamespaceResponse' {arn} -> arn) (\s@CreateNamespaceResponse' {} a -> s {arn = a} :: CreateNamespaceResponse)

-- | The Amazon Web Services Region; that you want to use for the free SPICE
-- capacity for the new namespace. This is set to the region that you run
-- CreateNamespace in.
createNamespaceResponse_capacityRegion :: Lens.Lens' CreateNamespaceResponse (Prelude.Maybe Prelude.Text)
createNamespaceResponse_capacityRegion = Lens.lens (\CreateNamespaceResponse' {capacityRegion} -> capacityRegion) (\s@CreateNamespaceResponse' {} a -> s {capacityRegion = a} :: CreateNamespaceResponse)

-- | The status of the creation of the namespace. This is an asynchronous
-- process. A status of @CREATED@ means that your namespace is ready to
-- use. If an error occurs, it indicates if the process is @retryable@ or
-- @non-retryable@. In the case of a non-retryable error, refer to the
-- error message for follow-up tasks.
createNamespaceResponse_creationStatus :: Lens.Lens' CreateNamespaceResponse (Prelude.Maybe NamespaceStatus)
createNamespaceResponse_creationStatus = Lens.lens (\CreateNamespaceResponse' {creationStatus} -> creationStatus) (\s@CreateNamespaceResponse' {} a -> s {creationStatus = a} :: CreateNamespaceResponse)

-- | Specifies the type of your user identity directory. Currently, this
-- supports users with an identity type of @QUICKSIGHT@.
createNamespaceResponse_identityStore :: Lens.Lens' CreateNamespaceResponse (Prelude.Maybe IdentityStore)
createNamespaceResponse_identityStore = Lens.lens (\CreateNamespaceResponse' {identityStore} -> identityStore) (\s@CreateNamespaceResponse' {} a -> s {identityStore = a} :: CreateNamespaceResponse)

-- | The name of the new namespace that you created.
createNamespaceResponse_name :: Lens.Lens' CreateNamespaceResponse (Prelude.Maybe Prelude.Text)
createNamespaceResponse_name = Lens.lens (\CreateNamespaceResponse' {name} -> name) (\s@CreateNamespaceResponse' {} a -> s {name = a} :: CreateNamespaceResponse)

-- | The Amazon Web Services request ID for this operation.
createNamespaceResponse_requestId :: Lens.Lens' CreateNamespaceResponse (Prelude.Maybe Prelude.Text)
createNamespaceResponse_requestId = Lens.lens (\CreateNamespaceResponse' {requestId} -> requestId) (\s@CreateNamespaceResponse' {} a -> s {requestId = a} :: CreateNamespaceResponse)

-- | The HTTP status of the request.
createNamespaceResponse_status :: Lens.Lens' CreateNamespaceResponse Prelude.Int
createNamespaceResponse_status = Lens.lens (\CreateNamespaceResponse' {status} -> status) (\s@CreateNamespaceResponse' {} a -> s {status = a} :: CreateNamespaceResponse)

instance Prelude.NFData CreateNamespaceResponse where
  rnf CreateNamespaceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf capacityRegion
      `Prelude.seq` Prelude.rnf creationStatus
      `Prelude.seq` Prelude.rnf identityStore
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
