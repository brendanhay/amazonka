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
-- Module      : Network.AWS.GameLift.CreateAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for a fleet. In most situations, you can use an alias
-- ID in place of a fleet ID. An alias provides a level of abstraction for
-- a fleet that is useful when redirecting player traffic from one fleet to
-- another, such as when updating your game build.
--
-- Amazon GameLift supports two types of routing strategies for aliases:
-- simple and terminal. A simple alias points to an active fleet. A
-- terminal alias is used to display messaging or link to a URL instead of
-- routing players to an active fleet. For example, you might use a
-- terminal alias when a game version is no longer supported and you want
-- to direct players to an upgrade site.
--
-- To create a fleet alias, specify an alias name, routing strategy, and
-- optional description. Each simple alias can point to only one fleet, but
-- a fleet can have multiple aliases. If successful, a new alias record is
-- returned, including an alias ID and an ARN. You can reassign an alias to
-- another fleet by calling @UpdateAlias@.
--
-- -   CreateAlias
--
-- -   ListAliases
--
-- -   DescribeAlias
--
-- -   UpdateAlias
--
-- -   DeleteAlias
--
-- -   ResolveAlias
module Network.AWS.GameLift.CreateAlias
  ( -- * Creating a Request
    CreateAlias (..),
    newCreateAlias,

    -- * Request Lenses
    createAlias_tags,
    createAlias_description,
    createAlias_name,
    createAlias_routingStrategy,

    -- * Destructuring the Response
    CreateAliasResponse (..),
    newCreateAliasResponse,

    -- * Response Lenses
    createAliasResponse_alias,
    createAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | A list of labels to assign to the new alias resource. Tags are
    -- developer-defined key-value pairs. Tagging AWS resources are useful for
    -- resource management, access management and cost allocation. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- in the /AWS General Reference/. Once the resource is created, you can
    -- use TagResource, UntagResource, and ListTagsForResource to add, remove,
    -- and view tags. The maximum tag limit may be lower than stated. See the
    -- AWS General Reference for actual tagging limits.
    tags :: Core.Maybe [Tag],
    -- | A human-readable description of the alias.
    description :: Core.Maybe Core.Text,
    -- | A descriptive label that is associated with an alias. Alias names do not
    -- need to be unique.
    name :: Core.Text,
    -- | The routing configuration, including routing type and fleet target, for
    -- the alias.
    routingStrategy :: RoutingStrategy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAlias_tags' - A list of labels to assign to the new alias resource. Tags are
-- developer-defined key-value pairs. Tagging AWS resources are useful for
-- resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
--
-- 'description', 'createAlias_description' - A human-readable description of the alias.
--
-- 'name', 'createAlias_name' - A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
--
-- 'routingStrategy', 'createAlias_routingStrategy' - The routing configuration, including routing type and fleet target, for
-- the alias.
newCreateAlias ::
  -- | 'name'
  Core.Text ->
  -- | 'routingStrategy'
  RoutingStrategy ->
  CreateAlias
newCreateAlias pName_ pRoutingStrategy_ =
  CreateAlias'
    { tags = Core.Nothing,
      description = Core.Nothing,
      name = pName_,
      routingStrategy = pRoutingStrategy_
    }

-- | A list of labels to assign to the new alias resource. Tags are
-- developer-defined key-value pairs. Tagging AWS resources are useful for
-- resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
createAlias_tags :: Lens.Lens' CreateAlias (Core.Maybe [Tag])
createAlias_tags = Lens.lens (\CreateAlias' {tags} -> tags) (\s@CreateAlias' {} a -> s {tags = a} :: CreateAlias) Core.. Lens.mapping Lens._Coerce

-- | A human-readable description of the alias.
createAlias_description :: Lens.Lens' CreateAlias (Core.Maybe Core.Text)
createAlias_description = Lens.lens (\CreateAlias' {description} -> description) (\s@CreateAlias' {} a -> s {description = a} :: CreateAlias)

-- | A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
createAlias_name :: Lens.Lens' CreateAlias Core.Text
createAlias_name = Lens.lens (\CreateAlias' {name} -> name) (\s@CreateAlias' {} a -> s {name = a} :: CreateAlias)

-- | The routing configuration, including routing type and fleet target, for
-- the alias.
createAlias_routingStrategy :: Lens.Lens' CreateAlias RoutingStrategy
createAlias_routingStrategy = Lens.lens (\CreateAlias' {routingStrategy} -> routingStrategy) (\s@CreateAlias' {} a -> s {routingStrategy = a} :: CreateAlias)

instance Core.AWSRequest CreateAlias where
  type AWSResponse CreateAlias = CreateAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAliasResponse'
            Core.<$> (x Core..?> "Alias")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateAlias

instance Core.NFData CreateAlias

instance Core.ToHeaders CreateAlias where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.CreateAlias" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just
              ("RoutingStrategy" Core..= routingStrategy)
          ]
      )

instance Core.ToPath CreateAlias where
  toPath = Core.const "/"

instance Core.ToQuery CreateAlias where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { -- | The newly created alias resource.
    alias :: Core.Maybe Alias,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'createAliasResponse_alias' - The newly created alias resource.
--
-- 'httpStatus', 'createAliasResponse_httpStatus' - The response's http status code.
newCreateAliasResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateAliasResponse
newCreateAliasResponse pHttpStatus_ =
  CreateAliasResponse'
    { alias = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created alias resource.
createAliasResponse_alias :: Lens.Lens' CreateAliasResponse (Core.Maybe Alias)
createAliasResponse_alias = Lens.lens (\CreateAliasResponse' {alias} -> alias) (\s@CreateAliasResponse' {} a -> s {alias = a} :: CreateAliasResponse)

-- | The response's http status code.
createAliasResponse_httpStatus :: Lens.Lens' CreateAliasResponse Core.Int
createAliasResponse_httpStatus = Lens.lens (\CreateAliasResponse' {httpStatus} -> httpStatus) (\s@CreateAliasResponse' {} a -> s {httpStatus = a} :: CreateAliasResponse)

instance Core.NFData CreateAliasResponse
