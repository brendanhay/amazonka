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
-- Module      : Amazonka.ECS.CreateCapacityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new capacity provider. Capacity providers are associated with
-- an Amazon ECS cluster and are used in capacity provider strategies to
-- facilitate cluster auto scaling.
--
-- Only capacity providers that use an Auto Scaling group can be created.
-- Amazon ECS tasks on Fargate use the @FARGATE@ and @FARGATE_SPOT@
-- capacity providers. These providers are available to all accounts in the
-- Amazon Web Services Regions that Fargate supports.
module Amazonka.ECS.CreateCapacityProvider
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCapacityProvider' smart constructor.
data CreateCapacityProvider = CreateCapacityProvider'
  { -- | The metadata that you apply to the capacity provider to categorize and
    -- organize them more conveniently. Each tag consists of a key and an
    -- optional value. You define both of them.
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
    -- | The name of the capacity provider. Up to 255 characters are allowed.
    -- They include letters (both upper and lowercase letters), numbers,
    -- underscores (_), and hyphens (-). The name can\'t be prefixed with
    -- \"@aws@\", \"@ecs@\", or \"@fargate@\".
    name :: Prelude.Text,
    -- | The details of the Auto Scaling group for the capacity provider.
    autoScalingGroupProvider :: AutoScalingGroupProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCapacityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCapacityProvider_tags' - The metadata that you apply to the capacity provider to categorize and
-- organize them more conveniently. Each tag consists of a key and an
-- optional value. You define both of them.
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
-- 'name', 'createCapacityProvider_name' - The name of the capacity provider. Up to 255 characters are allowed.
-- They include letters (both upper and lowercase letters), numbers,
-- underscores (_), and hyphens (-). The name can\'t be prefixed with
-- \"@aws@\", \"@ecs@\", or \"@fargate@\".
--
-- 'autoScalingGroupProvider', 'createCapacityProvider_autoScalingGroupProvider' - The details of the Auto Scaling group for the capacity provider.
newCreateCapacityProvider ::
  -- | 'name'
  Prelude.Text ->
  -- | 'autoScalingGroupProvider'
  AutoScalingGroupProvider ->
  CreateCapacityProvider
newCreateCapacityProvider
  pName_
  pAutoScalingGroupProvider_ =
    CreateCapacityProvider'
      { tags = Prelude.Nothing,
        name = pName_,
        autoScalingGroupProvider =
          pAutoScalingGroupProvider_
      }

-- | The metadata that you apply to the capacity provider to categorize and
-- organize them more conveniently. Each tag consists of a key and an
-- optional value. You define both of them.
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
createCapacityProvider_tags :: Lens.Lens' CreateCapacityProvider (Prelude.Maybe [Tag])
createCapacityProvider_tags = Lens.lens (\CreateCapacityProvider' {tags} -> tags) (\s@CreateCapacityProvider' {} a -> s {tags = a} :: CreateCapacityProvider) Prelude.. Lens.mapping Lens.coerced

-- | The name of the capacity provider. Up to 255 characters are allowed.
-- They include letters (both upper and lowercase letters), numbers,
-- underscores (_), and hyphens (-). The name can\'t be prefixed with
-- \"@aws@\", \"@ecs@\", or \"@fargate@\".
createCapacityProvider_name :: Lens.Lens' CreateCapacityProvider Prelude.Text
createCapacityProvider_name = Lens.lens (\CreateCapacityProvider' {name} -> name) (\s@CreateCapacityProvider' {} a -> s {name = a} :: CreateCapacityProvider)

-- | The details of the Auto Scaling group for the capacity provider.
createCapacityProvider_autoScalingGroupProvider :: Lens.Lens' CreateCapacityProvider AutoScalingGroupProvider
createCapacityProvider_autoScalingGroupProvider = Lens.lens (\CreateCapacityProvider' {autoScalingGroupProvider} -> autoScalingGroupProvider) (\s@CreateCapacityProvider' {} a -> s {autoScalingGroupProvider = a} :: CreateCapacityProvider)

instance Core.AWSRequest CreateCapacityProvider where
  type
    AWSResponse CreateCapacityProvider =
      CreateCapacityProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCapacityProviderResponse'
            Prelude.<$> (x Data..?> "capacityProvider")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCapacityProvider where
  hashWithSalt _salt CreateCapacityProvider' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` autoScalingGroupProvider

instance Prelude.NFData CreateCapacityProvider where
  rnf CreateCapacityProvider' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf autoScalingGroupProvider

instance Data.ToHeaders CreateCapacityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.CreateCapacityProvider" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCapacityProvider where
  toJSON CreateCapacityProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ( "autoScalingGroupProvider"
                  Data..= autoScalingGroupProvider
              )
          ]
      )

instance Data.ToPath CreateCapacityProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCapacityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCapacityProviderResponse' smart constructor.
data CreateCapacityProviderResponse = CreateCapacityProviderResponse'
  { -- | The full description of the new capacity provider.
    capacityProvider :: Prelude.Maybe CapacityProvider,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateCapacityProviderResponse
newCreateCapacityProviderResponse pHttpStatus_ =
  CreateCapacityProviderResponse'
    { capacityProvider =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of the new capacity provider.
createCapacityProviderResponse_capacityProvider :: Lens.Lens' CreateCapacityProviderResponse (Prelude.Maybe CapacityProvider)
createCapacityProviderResponse_capacityProvider = Lens.lens (\CreateCapacityProviderResponse' {capacityProvider} -> capacityProvider) (\s@CreateCapacityProviderResponse' {} a -> s {capacityProvider = a} :: CreateCapacityProviderResponse)

-- | The response's http status code.
createCapacityProviderResponse_httpStatus :: Lens.Lens' CreateCapacityProviderResponse Prelude.Int
createCapacityProviderResponse_httpStatus = Lens.lens (\CreateCapacityProviderResponse' {httpStatus} -> httpStatus) (\s@CreateCapacityProviderResponse' {} a -> s {httpStatus = a} :: CreateCapacityProviderResponse)

instance
  Prelude.NFData
    CreateCapacityProviderResponse
  where
  rnf CreateCapacityProviderResponse' {..} =
    Prelude.rnf capacityProvider
      `Prelude.seq` Prelude.rnf httpStatus
