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
    eLifecycle,
    eOwnerARN,
    eName,
    eId,
    eType,
    eConnectionType,
    eDescription,
  )
where

import Network.AWS.Cloud9.Types.ConnectionType
import Network.AWS.Cloud9.Types.EnvironmentLifecycle
import Network.AWS.Cloud9.Types.EnvironmentType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an AWS Cloud9 development environment.
--
-- /See:/ 'mkEnvironment' smart constructor.
data Environment = Environment'
  { -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Lude.Maybe Lude.Text,
    -- | The state of the environment in its creation or deletion lifecycle.
    lifecycle :: Lude.Maybe EnvironmentLifecycle,
    -- | The Amazon Resource Name (ARN) of the environment owner.
    ownerARN :: Lude.Maybe Lude.Text,
    -- | The name of the environment.
    name :: Lude.Maybe Lude.Text,
    -- | The ID of the environment.
    id :: Lude.Maybe Lude.Text,
    -- | The type of environment. Valid values include the following:
    --
    --
    --     * @ec2@ : An Amazon Elastic Compute Cloud (Amazon EC2) instance connects to the environment.
    --
    --
    --     * @ssh@ : Your own server connects to the environment.
    type' :: Lude.Maybe EnvironmentType,
    -- | The connection type used for connecting to an Amazon EC2 environment.
    connectionType :: Lude.Maybe ConnectionType,
    -- | The description for the environment.
    description :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Environment' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the environment.
-- * 'lifecycle' - The state of the environment in its creation or deletion lifecycle.
-- * 'ownerARN' - The Amazon Resource Name (ARN) of the environment owner.
-- * 'name' - The name of the environment.
-- * 'id' - The ID of the environment.
-- * 'type'' - The type of environment. Valid values include the following:
--
--
--     * @ec2@ : An Amazon Elastic Compute Cloud (Amazon EC2) instance connects to the environment.
--
--
--     * @ssh@ : Your own server connects to the environment.
--
--
-- * 'connectionType' - The connection type used for connecting to an Amazon EC2 environment.
-- * 'description' - The description for the environment.
mkEnvironment ::
  Environment
mkEnvironment =
  Environment'
    { arn = Lude.Nothing,
      lifecycle = Lude.Nothing,
      ownerARN = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      connectionType = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the environment.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eArn :: Lens.Lens' Environment (Lude.Maybe Lude.Text)
eArn = Lens.lens (arn :: Environment -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Environment)
{-# DEPRECATED eArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The state of the environment in its creation or deletion lifecycle.
--
-- /Note:/ Consider using 'lifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLifecycle :: Lens.Lens' Environment (Lude.Maybe EnvironmentLifecycle)
eLifecycle = Lens.lens (lifecycle :: Environment -> Lude.Maybe EnvironmentLifecycle) (\s a -> s {lifecycle = a} :: Environment)
{-# DEPRECATED eLifecycle "Use generic-lens or generic-optics with 'lifecycle' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment owner.
--
-- /Note:/ Consider using 'ownerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOwnerARN :: Lens.Lens' Environment (Lude.Maybe Lude.Text)
eOwnerARN = Lens.lens (ownerARN :: Environment -> Lude.Maybe Lude.Text) (\s a -> s {ownerARN = a} :: Environment)
{-# DEPRECATED eOwnerARN "Use generic-lens or generic-optics with 'ownerARN' instead." #-}

-- | The name of the environment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eName :: Lens.Lens' Environment (Lude.Maybe Lude.Text)
eName = Lens.lens (name :: Environment -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Environment)
{-# DEPRECATED eName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the environment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eId :: Lens.Lens' Environment (Lude.Maybe Lude.Text)
eId = Lens.lens (id :: Environment -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Environment)
{-# DEPRECATED eId "Use generic-lens or generic-optics with 'id' instead." #-}

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
eType :: Lens.Lens' Environment (Lude.Maybe EnvironmentType)
eType = Lens.lens (type' :: Environment -> Lude.Maybe EnvironmentType) (\s a -> s {type' = a} :: Environment)
{-# DEPRECATED eType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The connection type used for connecting to an Amazon EC2 environment.
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eConnectionType :: Lens.Lens' Environment (Lude.Maybe ConnectionType)
eConnectionType = Lens.lens (connectionType :: Environment -> Lude.Maybe ConnectionType) (\s a -> s {connectionType = a} :: Environment)
{-# DEPRECATED eConnectionType "Use generic-lens or generic-optics with 'connectionType' instead." #-}

-- | The description for the environment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDescription :: Lens.Lens' Environment (Lude.Maybe (Lude.Sensitive Lude.Text))
eDescription = Lens.lens (description :: Environment -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: Environment)
{-# DEPRECATED eDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Environment where
  parseJSON =
    Lude.withObject
      "Environment"
      ( \x ->
          Environment'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "lifecycle")
            Lude.<*> (x Lude..:? "ownerArn")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "connectionType")
            Lude.<*> (x Lude..:? "description")
      )
