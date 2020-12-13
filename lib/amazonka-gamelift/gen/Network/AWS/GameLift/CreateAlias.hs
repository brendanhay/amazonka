{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for a fleet. In most situations, you can use an alias ID in place of a fleet ID. An alias provides a level of abstraction for a fleet that is useful when redirecting player traffic from one fleet to another, such as when updating your game build.
--
-- Amazon GameLift supports two types of routing strategies for aliases: simple and terminal. A simple alias points to an active fleet. A terminal alias is used to display messaging or link to a URL instead of routing players to an active fleet. For example, you might use a terminal alias when a game version is no longer supported and you want to direct players to an upgrade site.
-- To create a fleet alias, specify an alias name, routing strategy, and optional description. Each simple alias can point to only one fleet, but a fleet can have multiple aliases. If successful, a new alias record is returned, including an alias ID and an ARN. You can reassign an alias to another fleet by calling @UpdateAlias@ .
--
--     * 'CreateAlias'
--
--
--     * 'ListAliases'
--
--
--     * 'DescribeAlias'
--
--
--     * 'UpdateAlias'
--
--
--     * 'DeleteAlias'
--
--
--     * 'ResolveAlias'
module Network.AWS.GameLift.CreateAlias
  ( -- * Creating a request
    CreateAlias (..),
    mkCreateAlias,

    -- ** Request lenses
    caRoutingStrategy,
    caName,
    caDescription,
    caTags,

    -- * Destructuring the response
    CreateAliasResponse (..),
    mkCreateAliasResponse,

    -- ** Response lenses
    carsAlias,
    carsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | The routing configuration, including routing type and fleet target, for the alias.
    routingStrategy :: RoutingStrategy,
    -- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
    name :: Lude.Text,
    -- | A human-readable description of the alias.
    description :: Lude.Maybe Lude.Text,
    -- | A list of labels to assign to the new alias resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- * 'routingStrategy' - The routing configuration, including routing type and fleet target, for the alias.
-- * 'name' - A descriptive label that is associated with an alias. Alias names do not need to be unique.
-- * 'description' - A human-readable description of the alias.
-- * 'tags' - A list of labels to assign to the new alias resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
mkCreateAlias ::
  -- | 'routingStrategy'
  RoutingStrategy ->
  -- | 'name'
  Lude.Text ->
  CreateAlias
mkCreateAlias pRoutingStrategy_ pName_ =
  CreateAlias'
    { routingStrategy = pRoutingStrategy_,
      name = pName_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The routing configuration, including routing type and fleet target, for the alias.
--
-- /Note:/ Consider using 'routingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRoutingStrategy :: Lens.Lens' CreateAlias RoutingStrategy
caRoutingStrategy = Lens.lens (routingStrategy :: CreateAlias -> RoutingStrategy) (\s a -> s {routingStrategy = a} :: CreateAlias)
{-# DEPRECATED caRoutingStrategy "Use generic-lens or generic-optics with 'routingStrategy' instead." #-}

-- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateAlias Lude.Text
caName = Lens.lens (name :: CreateAlias -> Lude.Text) (\s a -> s {name = a} :: CreateAlias)
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A human-readable description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateAlias (Lude.Maybe Lude.Text)
caDescription = Lens.lens (description :: CreateAlias -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateAlias)
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of labels to assign to the new alias resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateAlias (Lude.Maybe [Tag])
caTags = Lens.lens (tags :: CreateAlias -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateAlias)
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateAlias where
  type Rs CreateAlias = CreateAliasResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAliasResponse'
            Lude.<$> (x Lude..?> "Alias") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreateAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RoutingStrategy" Lude..= routingStrategy),
            Lude.Just ("Name" Lude..= name),
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAlias where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { -- | The newly created alias resource.
    alias :: Lude.Maybe Alias,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAliasResponse' with the minimum fields required to make a request.
--
-- * 'alias' - The newly created alias resource.
-- * 'responseStatus' - The response status code.
mkCreateAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAliasResponse
mkCreateAliasResponse pResponseStatus_ =
  CreateAliasResponse'
    { alias = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly created alias resource.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsAlias :: Lens.Lens' CreateAliasResponse (Lude.Maybe Alias)
carsAlias = Lens.lens (alias :: CreateAliasResponse -> Lude.Maybe Alias) (\s a -> s {alias = a} :: CreateAliasResponse)
{-# DEPRECATED carsAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateAliasResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAliasResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
