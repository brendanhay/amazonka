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
-- Module      : Amazonka.GameLift.CreateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.CreateAlias
  ( -- * Creating a Request
    CreateAlias (..),
    newCreateAlias,

    -- * Request Lenses
    createAlias_description,
    createAlias_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | A human-readable description of the alias.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of labels to assign to the new alias resource. Tags are
    -- developer-defined key-value pairs. Tagging Amazon Web Services resources
    -- are useful for resource management, access management and cost
    -- allocation. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in the /Amazon Web Services General Reference/.
    tags :: Prelude.Maybe [Tag],
    -- | A descriptive label that is associated with an alias. Alias names do not
    -- need to be unique.
    name :: Prelude.Text,
    -- | The routing configuration, including routing type and fleet target, for
    -- the alias.
    routingStrategy :: RoutingStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createAlias_description' - A human-readable description of the alias.
--
-- 'tags', 'createAlias_tags' - A list of labels to assign to the new alias resource. Tags are
-- developer-defined key-value pairs. Tagging Amazon Web Services resources
-- are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/.
--
-- 'name', 'createAlias_name' - A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
--
-- 'routingStrategy', 'createAlias_routingStrategy' - The routing configuration, including routing type and fleet target, for
-- the alias.
newCreateAlias ::
  -- | 'name'
  Prelude.Text ->
  -- | 'routingStrategy'
  RoutingStrategy ->
  CreateAlias
newCreateAlias pName_ pRoutingStrategy_ =
  CreateAlias'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      routingStrategy = pRoutingStrategy_
    }

-- | A human-readable description of the alias.
createAlias_description :: Lens.Lens' CreateAlias (Prelude.Maybe Prelude.Text)
createAlias_description = Lens.lens (\CreateAlias' {description} -> description) (\s@CreateAlias' {} a -> s {description = a} :: CreateAlias)

-- | A list of labels to assign to the new alias resource. Tags are
-- developer-defined key-value pairs. Tagging Amazon Web Services resources
-- are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/.
createAlias_tags :: Lens.Lens' CreateAlias (Prelude.Maybe [Tag])
createAlias_tags = Lens.lens (\CreateAlias' {tags} -> tags) (\s@CreateAlias' {} a -> s {tags = a} :: CreateAlias) Prelude.. Lens.mapping Lens.coerced

-- | A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
createAlias_name :: Lens.Lens' CreateAlias Prelude.Text
createAlias_name = Lens.lens (\CreateAlias' {name} -> name) (\s@CreateAlias' {} a -> s {name = a} :: CreateAlias)

-- | The routing configuration, including routing type and fleet target, for
-- the alias.
createAlias_routingStrategy :: Lens.Lens' CreateAlias RoutingStrategy
createAlias_routingStrategy = Lens.lens (\CreateAlias' {routingStrategy} -> routingStrategy) (\s@CreateAlias' {} a -> s {routingStrategy = a} :: CreateAlias)

instance Core.AWSRequest CreateAlias where
  type AWSResponse CreateAlias = CreateAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAliasResponse'
            Prelude.<$> (x Data..?> "Alias")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAlias where
  hashWithSalt _salt CreateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` routingStrategy

instance Prelude.NFData CreateAlias where
  rnf CreateAlias' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf routingStrategy

instance Data.ToHeaders CreateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.CreateAlias" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("RoutingStrategy" Data..= routingStrategy)
          ]
      )

instance Data.ToPath CreateAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { -- | The newly created alias resource.
    alias :: Prelude.Maybe Alias,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateAliasResponse
newCreateAliasResponse pHttpStatus_ =
  CreateAliasResponse'
    { alias = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created alias resource.
createAliasResponse_alias :: Lens.Lens' CreateAliasResponse (Prelude.Maybe Alias)
createAliasResponse_alias = Lens.lens (\CreateAliasResponse' {alias} -> alias) (\s@CreateAliasResponse' {} a -> s {alias = a} :: CreateAliasResponse)

-- | The response's http status code.
createAliasResponse_httpStatus :: Lens.Lens' CreateAliasResponse Prelude.Int
createAliasResponse_httpStatus = Lens.lens (\CreateAliasResponse' {httpStatus} -> httpStatus) (\s@CreateAliasResponse' {} a -> s {httpStatus = a} :: CreateAliasResponse)

instance Prelude.NFData CreateAliasResponse where
  rnf CreateAliasResponse' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf httpStatus
