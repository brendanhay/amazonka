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
-- Module      : Amazonka.ECS.RegisterContainerInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon ECS agent, and it is not intended
-- for use outside of the agent.
--
-- Registers an EC2 instance into the specified cluster. This instance
-- becomes available to place containers on.
module Amazonka.ECS.RegisterContainerInstance
  ( -- * Creating a Request
    RegisterContainerInstance (..),
    newRegisterContainerInstance,

    -- * Request Lenses
    registerContainerInstance_attributes,
    registerContainerInstance_cluster,
    registerContainerInstance_containerInstanceArn,
    registerContainerInstance_instanceIdentityDocument,
    registerContainerInstance_instanceIdentityDocumentSignature,
    registerContainerInstance_platformDevices,
    registerContainerInstance_tags,
    registerContainerInstance_totalResources,
    registerContainerInstance_versionInfo,

    -- * Destructuring the Response
    RegisterContainerInstanceResponse (..),
    newRegisterContainerInstanceResponse,

    -- * Response Lenses
    registerContainerInstanceResponse_containerInstance,
    registerContainerInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterContainerInstance' smart constructor.
data RegisterContainerInstance = RegisterContainerInstance'
  { -- | The container instance attributes that this container instance supports.
    attributes :: Prelude.Maybe [Attribute],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster to
    -- register your container instance with. If you do not specify a cluster,
    -- the default cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the container instance (if it was previously registered).
    containerInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The instance identity document for the EC2 instance to register. This
    -- document can be found by running the following command from the
    -- instance:
    -- @curl http:\/\/169.254.169.254\/latest\/dynamic\/instance-identity\/document\/@
    instanceIdentityDocument :: Prelude.Maybe Prelude.Text,
    -- | The instance identity document signature for the EC2 instance to
    -- register. This signature can be found by running the following command
    -- from the instance:
    -- @curl http:\/\/169.254.169.254\/latest\/dynamic\/instance-identity\/signature\/@
    instanceIdentityDocumentSignature :: Prelude.Maybe Prelude.Text,
    -- | The devices that are available on the container instance. The only
    -- supported device type is a GPU.
    platformDevices :: Prelude.Maybe [PlatformDevice],
    -- | The metadata that you apply to the container instance to help you
    -- categorize and organize them. Each tag consists of a key and an optional
    -- value. You define both.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case-sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for either keys or values as it is reserved for
    --     Amazon Web Services use. You cannot edit or delete tag keys or
    --     values with this prefix. Tags with this prefix do not count against
    --     your tags per resource limit.
    tags :: Prelude.Maybe [Tag],
    -- | The resources available on the instance.
    totalResources :: Prelude.Maybe [Resource],
    -- | The version information for the Amazon ECS container agent and Docker
    -- daemon that runs on the container instance.
    versionInfo :: Prelude.Maybe VersionInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterContainerInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'registerContainerInstance_attributes' - The container instance attributes that this container instance supports.
--
-- 'cluster', 'registerContainerInstance_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster to
-- register your container instance with. If you do not specify a cluster,
-- the default cluster is assumed.
--
-- 'containerInstanceArn', 'registerContainerInstance_containerInstanceArn' - The ARN of the container instance (if it was previously registered).
--
-- 'instanceIdentityDocument', 'registerContainerInstance_instanceIdentityDocument' - The instance identity document for the EC2 instance to register. This
-- document can be found by running the following command from the
-- instance:
-- @curl http:\/\/169.254.169.254\/latest\/dynamic\/instance-identity\/document\/@
--
-- 'instanceIdentityDocumentSignature', 'registerContainerInstance_instanceIdentityDocumentSignature' - The instance identity document signature for the EC2 instance to
-- register. This signature can be found by running the following command
-- from the instance:
-- @curl http:\/\/169.254.169.254\/latest\/dynamic\/instance-identity\/signature\/@
--
-- 'platformDevices', 'registerContainerInstance_platformDevices' - The devices that are available on the container instance. The only
-- supported device type is a GPU.
--
-- 'tags', 'registerContainerInstance_tags' - The metadata that you apply to the container instance to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value. You define both.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
--
-- 'totalResources', 'registerContainerInstance_totalResources' - The resources available on the instance.
--
-- 'versionInfo', 'registerContainerInstance_versionInfo' - The version information for the Amazon ECS container agent and Docker
-- daemon that runs on the container instance.
newRegisterContainerInstance ::
  RegisterContainerInstance
newRegisterContainerInstance =
  RegisterContainerInstance'
    { attributes =
        Prelude.Nothing,
      cluster = Prelude.Nothing,
      containerInstanceArn = Prelude.Nothing,
      instanceIdentityDocument = Prelude.Nothing,
      instanceIdentityDocumentSignature =
        Prelude.Nothing,
      platformDevices = Prelude.Nothing,
      tags = Prelude.Nothing,
      totalResources = Prelude.Nothing,
      versionInfo = Prelude.Nothing
    }

-- | The container instance attributes that this container instance supports.
registerContainerInstance_attributes :: Lens.Lens' RegisterContainerInstance (Prelude.Maybe [Attribute])
registerContainerInstance_attributes = Lens.lens (\RegisterContainerInstance' {attributes} -> attributes) (\s@RegisterContainerInstance' {} a -> s {attributes = a} :: RegisterContainerInstance) Prelude.. Lens.mapping Lens.coerced

-- | The short name or full Amazon Resource Name (ARN) of the cluster to
-- register your container instance with. If you do not specify a cluster,
-- the default cluster is assumed.
registerContainerInstance_cluster :: Lens.Lens' RegisterContainerInstance (Prelude.Maybe Prelude.Text)
registerContainerInstance_cluster = Lens.lens (\RegisterContainerInstance' {cluster} -> cluster) (\s@RegisterContainerInstance' {} a -> s {cluster = a} :: RegisterContainerInstance)

-- | The ARN of the container instance (if it was previously registered).
registerContainerInstance_containerInstanceArn :: Lens.Lens' RegisterContainerInstance (Prelude.Maybe Prelude.Text)
registerContainerInstance_containerInstanceArn = Lens.lens (\RegisterContainerInstance' {containerInstanceArn} -> containerInstanceArn) (\s@RegisterContainerInstance' {} a -> s {containerInstanceArn = a} :: RegisterContainerInstance)

-- | The instance identity document for the EC2 instance to register. This
-- document can be found by running the following command from the
-- instance:
-- @curl http:\/\/169.254.169.254\/latest\/dynamic\/instance-identity\/document\/@
registerContainerInstance_instanceIdentityDocument :: Lens.Lens' RegisterContainerInstance (Prelude.Maybe Prelude.Text)
registerContainerInstance_instanceIdentityDocument = Lens.lens (\RegisterContainerInstance' {instanceIdentityDocument} -> instanceIdentityDocument) (\s@RegisterContainerInstance' {} a -> s {instanceIdentityDocument = a} :: RegisterContainerInstance)

-- | The instance identity document signature for the EC2 instance to
-- register. This signature can be found by running the following command
-- from the instance:
-- @curl http:\/\/169.254.169.254\/latest\/dynamic\/instance-identity\/signature\/@
registerContainerInstance_instanceIdentityDocumentSignature :: Lens.Lens' RegisterContainerInstance (Prelude.Maybe Prelude.Text)
registerContainerInstance_instanceIdentityDocumentSignature = Lens.lens (\RegisterContainerInstance' {instanceIdentityDocumentSignature} -> instanceIdentityDocumentSignature) (\s@RegisterContainerInstance' {} a -> s {instanceIdentityDocumentSignature = a} :: RegisterContainerInstance)

-- | The devices that are available on the container instance. The only
-- supported device type is a GPU.
registerContainerInstance_platformDevices :: Lens.Lens' RegisterContainerInstance (Prelude.Maybe [PlatformDevice])
registerContainerInstance_platformDevices = Lens.lens (\RegisterContainerInstance' {platformDevices} -> platformDevices) (\s@RegisterContainerInstance' {} a -> s {platformDevices = a} :: RegisterContainerInstance) Prelude.. Lens.mapping Lens.coerced

-- | The metadata that you apply to the container instance to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value. You define both.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
registerContainerInstance_tags :: Lens.Lens' RegisterContainerInstance (Prelude.Maybe [Tag])
registerContainerInstance_tags = Lens.lens (\RegisterContainerInstance' {tags} -> tags) (\s@RegisterContainerInstance' {} a -> s {tags = a} :: RegisterContainerInstance) Prelude.. Lens.mapping Lens.coerced

-- | The resources available on the instance.
registerContainerInstance_totalResources :: Lens.Lens' RegisterContainerInstance (Prelude.Maybe [Resource])
registerContainerInstance_totalResources = Lens.lens (\RegisterContainerInstance' {totalResources} -> totalResources) (\s@RegisterContainerInstance' {} a -> s {totalResources = a} :: RegisterContainerInstance) Prelude.. Lens.mapping Lens.coerced

-- | The version information for the Amazon ECS container agent and Docker
-- daemon that runs on the container instance.
registerContainerInstance_versionInfo :: Lens.Lens' RegisterContainerInstance (Prelude.Maybe VersionInfo)
registerContainerInstance_versionInfo = Lens.lens (\RegisterContainerInstance' {versionInfo} -> versionInfo) (\s@RegisterContainerInstance' {} a -> s {versionInfo = a} :: RegisterContainerInstance)

instance Core.AWSRequest RegisterContainerInstance where
  type
    AWSResponse RegisterContainerInstance =
      RegisterContainerInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterContainerInstanceResponse'
            Prelude.<$> (x Data..?> "containerInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterContainerInstance where
  hashWithSalt _salt RegisterContainerInstance' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` containerInstanceArn
      `Prelude.hashWithSalt` instanceIdentityDocument
      `Prelude.hashWithSalt` instanceIdentityDocumentSignature
      `Prelude.hashWithSalt` platformDevices
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` totalResources
      `Prelude.hashWithSalt` versionInfo

instance Prelude.NFData RegisterContainerInstance where
  rnf RegisterContainerInstance' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf containerInstanceArn
      `Prelude.seq` Prelude.rnf instanceIdentityDocument
      `Prelude.seq` Prelude.rnf instanceIdentityDocumentSignature
      `Prelude.seq` Prelude.rnf platformDevices
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf totalResources
      `Prelude.seq` Prelude.rnf versionInfo

instance Data.ToHeaders RegisterContainerInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.RegisterContainerInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterContainerInstance where
  toJSON RegisterContainerInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("cluster" Data..=) Prelude.<$> cluster,
            ("containerInstanceArn" Data..=)
              Prelude.<$> containerInstanceArn,
            ("instanceIdentityDocument" Data..=)
              Prelude.<$> instanceIdentityDocument,
            ("instanceIdentityDocumentSignature" Data..=)
              Prelude.<$> instanceIdentityDocumentSignature,
            ("platformDevices" Data..=)
              Prelude.<$> platformDevices,
            ("tags" Data..=) Prelude.<$> tags,
            ("totalResources" Data..=)
              Prelude.<$> totalResources,
            ("versionInfo" Data..=) Prelude.<$> versionInfo
          ]
      )

instance Data.ToPath RegisterContainerInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterContainerInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterContainerInstanceResponse' smart constructor.
data RegisterContainerInstanceResponse = RegisterContainerInstanceResponse'
  { -- | The container instance that was registered.
    containerInstance :: Prelude.Maybe ContainerInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterContainerInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerInstance', 'registerContainerInstanceResponse_containerInstance' - The container instance that was registered.
--
-- 'httpStatus', 'registerContainerInstanceResponse_httpStatus' - The response's http status code.
newRegisterContainerInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterContainerInstanceResponse
newRegisterContainerInstanceResponse pHttpStatus_ =
  RegisterContainerInstanceResponse'
    { containerInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The container instance that was registered.
registerContainerInstanceResponse_containerInstance :: Lens.Lens' RegisterContainerInstanceResponse (Prelude.Maybe ContainerInstance)
registerContainerInstanceResponse_containerInstance = Lens.lens (\RegisterContainerInstanceResponse' {containerInstance} -> containerInstance) (\s@RegisterContainerInstanceResponse' {} a -> s {containerInstance = a} :: RegisterContainerInstanceResponse)

-- | The response's http status code.
registerContainerInstanceResponse_httpStatus :: Lens.Lens' RegisterContainerInstanceResponse Prelude.Int
registerContainerInstanceResponse_httpStatus = Lens.lens (\RegisterContainerInstanceResponse' {httpStatus} -> httpStatus) (\s@RegisterContainerInstanceResponse' {} a -> s {httpStatus = a} :: RegisterContainerInstanceResponse)

instance
  Prelude.NFData
    RegisterContainerInstanceResponse
  where
  rnf RegisterContainerInstanceResponse' {..} =
    Prelude.rnf containerInstance
      `Prelude.seq` Prelude.rnf httpStatus
