{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Secret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Secret
  ( Secret (..),

    -- * Smart constructor
    mkSecret,

    -- * Lenses
    sName,
    sValueFrom,
  )
where

import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the secret to expose to your container. Secrets can be exposed to a container in the following ways:
--
--
--     * To inject sensitive data into your containers as environment variables, use the @secrets@ container definition parameter.
--
--
--     * To reference sensitive information in the log configuration of a container, use the @secretOptions@ container definition parameter.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkSecret' smart constructor.
data Secret = Secret'
  { -- | The name of the secret.
    name :: Types.String,
    -- | The secret to expose to the container. The supported values are either the full ARN of the AWS Secrets Manager secret or the full ARN of the parameter in the AWS Systems Manager Parameter Store.
    valueFrom :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Secret' value with any optional fields omitted.
mkSecret ::
  -- | 'name'
  Types.String ->
  -- | 'valueFrom'
  Types.String ->
  Secret
mkSecret name valueFrom = Secret' {name, valueFrom}

-- | The name of the secret.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Secret Types.String
sName = Lens.field @"name"
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The secret to expose to the container. The supported values are either the full ARN of the AWS Secrets Manager secret or the full ARN of the parameter in the AWS Systems Manager Parameter Store.
--
-- /Note:/ Consider using 'valueFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sValueFrom :: Lens.Lens' Secret Types.String
sValueFrom = Lens.field @"valueFrom"
{-# DEPRECATED sValueFrom "Use generic-lens or generic-optics with 'valueFrom' instead." #-}

instance Core.FromJSON Secret where
  toJSON Secret {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("valueFrom" Core..= valueFrom)
          ]
      )

instance Core.FromJSON Secret where
  parseJSON =
    Core.withObject "Secret" Core.$
      \x ->
        Secret'
          Core.<$> (x Core..: "name") Core.<*> (x Core..: "valueFrom")
