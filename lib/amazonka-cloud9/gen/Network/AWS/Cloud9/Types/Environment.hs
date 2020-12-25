{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.Environment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.Environment
  ( Environment (..),

    -- * Smart constructor
    mkEnvironment,

    -- * Lenses
    eArn,
    eConnectionType,
    eDescription,
    eId,
    eLifecycle,
    eName,
    eOwnerArn,
    eType,
  )
where

import qualified Network.AWS.Cloud9.Types.Arn as Types
import qualified Network.AWS.Cloud9.Types.ConnectionType as Types
import qualified Network.AWS.Cloud9.Types.Description as Types
import qualified Network.AWS.Cloud9.Types.EnvironmentLifecycle as Types
import qualified Network.AWS.Cloud9.Types.EnvironmentType as Types
import qualified Network.AWS.Cloud9.Types.Id as Types
import qualified Network.AWS.Cloud9.Types.Name as Types
import qualified Network.AWS.Cloud9.Types.OwnerArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an AWS Cloud9 development environment.
--
-- /See:/ 'mkEnvironment' smart constructor.
data Environment = Environment'
  { -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Core.Maybe Types.Arn,
    -- | The connection type used for connecting to an Amazon EC2 environment.
    connectionType :: Core.Maybe Types.ConnectionType,
    -- | The description for the environment.
    description :: Core.Maybe Types.Description,
    -- | The ID of the environment.
    id :: Core.Maybe Types.Id,
    -- | The state of the environment in its creation or deletion lifecycle.
    lifecycle :: Core.Maybe Types.EnvironmentLifecycle,
    -- | The name of the environment.
    name :: Core.Maybe Types.Name,
    -- | The Amazon Resource Name (ARN) of the environment owner.
    ownerArn :: Core.Maybe Types.OwnerArn,
    -- | The type of environment. Valid values include the following:
    --
    --
    --     * @ec2@ : An Amazon Elastic Compute Cloud (Amazon EC2) instance connects to the environment.
    --
    --
    --     * @ssh@ : Your own server connects to the environment.
    type' :: Core.Maybe Types.EnvironmentType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Environment' value with any optional fields omitted.
mkEnvironment ::
  Environment
mkEnvironment =
  Environment'
    { arn = Core.Nothing,
      connectionType = Core.Nothing,
      description = Core.Nothing,
      id = Core.Nothing,
      lifecycle = Core.Nothing,
      name = Core.Nothing,
      ownerArn = Core.Nothing,
      type' = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the environment.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eArn :: Lens.Lens' Environment (Core.Maybe Types.Arn)
eArn = Lens.field @"arn"
{-# DEPRECATED eArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The connection type used for connecting to an Amazon EC2 environment.
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eConnectionType :: Lens.Lens' Environment (Core.Maybe Types.ConnectionType)
eConnectionType = Lens.field @"connectionType"
{-# DEPRECATED eConnectionType "Use generic-lens or generic-optics with 'connectionType' instead." #-}

-- | The description for the environment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDescription :: Lens.Lens' Environment (Core.Maybe Types.Description)
eDescription = Lens.field @"description"
{-# DEPRECATED eDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the environment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eId :: Lens.Lens' Environment (Core.Maybe Types.Id)
eId = Lens.field @"id"
{-# DEPRECATED eId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The state of the environment in its creation or deletion lifecycle.
--
-- /Note:/ Consider using 'lifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLifecycle :: Lens.Lens' Environment (Core.Maybe Types.EnvironmentLifecycle)
eLifecycle = Lens.field @"lifecycle"
{-# DEPRECATED eLifecycle "Use generic-lens or generic-optics with 'lifecycle' instead." #-}

-- | The name of the environment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eName :: Lens.Lens' Environment (Core.Maybe Types.Name)
eName = Lens.field @"name"
{-# DEPRECATED eName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment owner.
--
-- /Note:/ Consider using 'ownerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOwnerArn :: Lens.Lens' Environment (Core.Maybe Types.OwnerArn)
eOwnerArn = Lens.field @"ownerArn"
{-# DEPRECATED eOwnerArn "Use generic-lens or generic-optics with 'ownerArn' instead." #-}

-- | The type of environment. Valid values include the following:
--
--
--     * @ec2@ : An Amazon Elastic Compute Cloud (Amazon EC2) instance connects to the environment.
--
--
--     * @ssh@ : Your own server connects to the environment.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eType :: Lens.Lens' Environment (Core.Maybe Types.EnvironmentType)
eType = Lens.field @"type'"
{-# DEPRECATED eType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Environment where
  parseJSON =
    Core.withObject "Environment" Core.$
      \x ->
        Environment'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "connectionType")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "lifecycle")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "ownerArn")
          Core.<*> (x Core..:? "type")
