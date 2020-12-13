{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Secret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Secret
  ( Secret (..),

    -- * Smart constructor
    mkSecret,

    -- * Lenses
    sName,
    sValueFrom,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
    name :: Lude.Text,
    -- | The secret to expose to the container. The supported values are either the full ARN of the AWS Secrets Manager secret or the full ARN of the parameter in the AWS Systems Manager Parameter Store.
    valueFrom :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Secret' with the minimum fields required to make a request.
--
-- * 'name' - The name of the secret.
-- * 'valueFrom' - The secret to expose to the container. The supported values are either the full ARN of the AWS Secrets Manager secret or the full ARN of the parameter in the AWS Systems Manager Parameter Store.
mkSecret ::
  -- | 'name'
  Lude.Text ->
  -- | 'valueFrom'
  Lude.Text ->
  Secret
mkSecret pName_ pValueFrom_ =
  Secret' {name = pName_, valueFrom = pValueFrom_}

-- | The name of the secret.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Secret Lude.Text
sName = Lens.lens (name :: Secret -> Lude.Text) (\s a -> s {name = a} :: Secret)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The secret to expose to the container. The supported values are either the full ARN of the AWS Secrets Manager secret or the full ARN of the parameter in the AWS Systems Manager Parameter Store.
--
-- /Note:/ Consider using 'valueFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sValueFrom :: Lens.Lens' Secret Lude.Text
sValueFrom = Lens.lens (valueFrom :: Secret -> Lude.Text) (\s a -> s {valueFrom = a} :: Secret)
{-# DEPRECATED sValueFrom "Use generic-lens or generic-optics with 'valueFrom' instead." #-}

instance Lude.FromJSON Secret where
  parseJSON =
    Lude.withObject
      "Secret"
      ( \x ->
          Secret'
            Lude.<$> (x Lude..: "name") Lude.<*> (x Lude..: "valueFrom")
      )

instance Lude.ToJSON Secret where
  toJSON Secret' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("valueFrom" Lude..= valueFrom)
          ]
      )
