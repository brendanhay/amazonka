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
-- Module      : Network.AWS.ECS.CreateCapacityProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new capacity provider. Capacity providers are associated with
-- an Amazon ECS cluster and are used in capacity provider strategies to
-- facilitate cluster auto scaling.
--
-- Only capacity providers using an Auto Scaling group can be created.
-- Amazon ECS tasks on AWS Fargate use the @FARGATE@ and @FARGATE_SPOT@
-- capacity providers which are already created and available to all
-- accounts in Regions supported by AWS Fargate.
module Network.AWS.ECS.CreateCapacityProvider
  ( -- * Creating a Request
    CreateCapacityProvider (..),
    newCreateCapacityProvider,

    -- * Request Lenses
    createCapacityProvider_tags,
    createCapacityProvider_name,
    createCapacityProvider_autoScalingGroupProvider,

    -- * Destructuring the Response
    CreateCapacityProviderResponse (..),
    newCreateCapacityProviderResponse,

    -- * Response Lenses
    createCapacityProviderResponse_capacityProvider,
    createCapacityProviderResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCapacityProvider' smart constructor.
data CreateCapacityProvider = CreateCapacityProvider'
  { -- | The metadata that you apply to the capacity provider to help you
    -- categorize and organize them. Each tag consists of a key and an optional
    -- value, both of which you define.
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
    --     such as a prefix for either keys or values as it is reserved for AWS
    --     use. You cannot edit or delete tag keys or values with this prefix.
    --     Tags with this prefix do not count against your tags per resource
    --     limit.
    tags :: Core.Maybe [Tag],
    -- | The name of the capacity provider. Up to 255 characters are allowed,
    -- including letters (upper and lowercase), numbers, underscores, and
    -- hyphens. The name cannot be prefixed with \"@aws@\", \"@ecs@\", or
    -- \"@fargate@\".
    name :: Core.Text,
    -- | The details of the Auto Scaling group for the capacity provider.
    autoScalingGroupProvider :: AutoScalingGroupProvider
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCapacityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCapacityProvider_tags' - The metadata that you apply to the capacity provider to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
--
-- 'name', 'createCapacityProvider_name' - The name of the capacity provider. Up to 255 characters are allowed,
-- including letters (upper and lowercase), numbers, underscores, and
-- hyphens. The name cannot be prefixed with \"@aws@\", \"@ecs@\", or
-- \"@fargate@\".
--
-- 'autoScalingGroupProvider', 'createCapacityProvider_autoScalingGroupProvider' - The details of the Auto Scaling group for the capacity provider.
newCreateCapacityProvider ::
  -- | 'name'
  Core.Text ->
  -- | 'autoScalingGroupProvider'
  AutoScalingGroupProvider ->
  CreateCapacityProvider
newCreateCapacityProvider
  pName_
  pAutoScalingGroupProvider_ =
    CreateCapacityProvider'
      { tags = Core.Nothing,
        name = pName_,
        autoScalingGroupProvider =
          pAutoScalingGroupProvider_
      }

-- | The metadata that you apply to the capacity provider to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
createCapacityProvider_tags :: Lens.Lens' CreateCapacityProvider (Core.Maybe [Tag])
createCapacityProvider_tags = Lens.lens (\CreateCapacityProvider' {tags} -> tags) (\s@CreateCapacityProvider' {} a -> s {tags = a} :: CreateCapacityProvider) Core.. Lens.mapping Lens._Coerce

-- | The name of the capacity provider. Up to 255 characters are allowed,
-- including letters (upper and lowercase), numbers, underscores, and
-- hyphens. The name cannot be prefixed with \"@aws@\", \"@ecs@\", or
-- \"@fargate@\".
createCapacityProvider_name :: Lens.Lens' CreateCapacityProvider Core.Text
createCapacityProvider_name = Lens.lens (\CreateCapacityProvider' {name} -> name) (\s@CreateCapacityProvider' {} a -> s {name = a} :: CreateCapacityProvider)

-- | The details of the Auto Scaling group for the capacity provider.
createCapacityProvider_autoScalingGroupProvider :: Lens.Lens' CreateCapacityProvider AutoScalingGroupProvider
createCapacityProvider_autoScalingGroupProvider = Lens.lens (\CreateCapacityProvider' {autoScalingGroupProvider} -> autoScalingGroupProvider) (\s@CreateCapacityProvider' {} a -> s {autoScalingGroupProvider = a} :: CreateCapacityProvider)

instance Core.AWSRequest CreateCapacityProvider where
  type
    AWSResponse CreateCapacityProvider =
      CreateCapacityProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCapacityProviderResponse'
            Core.<$> (x Core..?> "capacityProvider")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCapacityProvider

instance Core.NFData CreateCapacityProvider

instance Core.ToHeaders CreateCapacityProvider where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.CreateCapacityProvider" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateCapacityProvider where
  toJSON CreateCapacityProvider' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            Core.Just ("name" Core..= name),
            Core.Just
              ( "autoScalingGroupProvider"
                  Core..= autoScalingGroupProvider
              )
          ]
      )

instance Core.ToPath CreateCapacityProvider where
  toPath = Core.const "/"

instance Core.ToQuery CreateCapacityProvider where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateCapacityProviderResponse' smart constructor.
data CreateCapacityProviderResponse = CreateCapacityProviderResponse'
  { -- | The full description of the new capacity provider.
    capacityProvider :: Core.Maybe CapacityProvider,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCapacityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProvider', 'createCapacityProviderResponse_capacityProvider' - The full description of the new capacity provider.
--
-- 'httpStatus', 'createCapacityProviderResponse_httpStatus' - The response's http status code.
newCreateCapacityProviderResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCapacityProviderResponse
newCreateCapacityProviderResponse pHttpStatus_ =
  CreateCapacityProviderResponse'
    { capacityProvider =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of the new capacity provider.
createCapacityProviderResponse_capacityProvider :: Lens.Lens' CreateCapacityProviderResponse (Core.Maybe CapacityProvider)
createCapacityProviderResponse_capacityProvider = Lens.lens (\CreateCapacityProviderResponse' {capacityProvider} -> capacityProvider) (\s@CreateCapacityProviderResponse' {} a -> s {capacityProvider = a} :: CreateCapacityProviderResponse)

-- | The response's http status code.
createCapacityProviderResponse_httpStatus :: Lens.Lens' CreateCapacityProviderResponse Core.Int
createCapacityProviderResponse_httpStatus = Lens.lens (\CreateCapacityProviderResponse' {httpStatus} -> httpStatus) (\s@CreateCapacityProviderResponse' {} a -> s {httpStatus = a} :: CreateCapacityProviderResponse)

instance Core.NFData CreateCapacityProviderResponse
