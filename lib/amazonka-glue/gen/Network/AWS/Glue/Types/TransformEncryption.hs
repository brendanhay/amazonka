{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformEncryption
  ( TransformEncryption (..),

    -- * Smart constructor
    mkTransformEncryption,

    -- * Lenses
    teMlUserDataEncryption,
    teTaskRunSecurityConfigurationName,
  )
where

import Network.AWS.Glue.Types.MLUserDataEncryption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
--
-- Additionally, imported labels and trained transforms can now be encrypted using a customer provided KMS key.
--
-- /See:/ 'mkTransformEncryption' smart constructor.
data TransformEncryption = TransformEncryption'
  { mlUserDataEncryption ::
      Lude.Maybe MLUserDataEncryption,
    taskRunSecurityConfigurationName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransformEncryption' with the minimum fields required to make a request.
--
-- * 'mlUserDataEncryption' - An @MLUserDataEncryption@ object containing the encryption mode and customer-provided KMS key ID.
-- * 'taskRunSecurityConfigurationName' - The name of the security configuration.
mkTransformEncryption ::
  TransformEncryption
mkTransformEncryption =
  TransformEncryption'
    { mlUserDataEncryption = Lude.Nothing,
      taskRunSecurityConfigurationName = Lude.Nothing
    }

-- | An @MLUserDataEncryption@ object containing the encryption mode and customer-provided KMS key ID.
--
-- /Note:/ Consider using 'mlUserDataEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teMlUserDataEncryption :: Lens.Lens' TransformEncryption (Lude.Maybe MLUserDataEncryption)
teMlUserDataEncryption = Lens.lens (mlUserDataEncryption :: TransformEncryption -> Lude.Maybe MLUserDataEncryption) (\s a -> s {mlUserDataEncryption = a} :: TransformEncryption)
{-# DEPRECATED teMlUserDataEncryption "Use generic-lens or generic-optics with 'mlUserDataEncryption' instead." #-}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'taskRunSecurityConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teTaskRunSecurityConfigurationName :: Lens.Lens' TransformEncryption (Lude.Maybe Lude.Text)
teTaskRunSecurityConfigurationName = Lens.lens (taskRunSecurityConfigurationName :: TransformEncryption -> Lude.Maybe Lude.Text) (\s a -> s {taskRunSecurityConfigurationName = a} :: TransformEncryption)
{-# DEPRECATED teTaskRunSecurityConfigurationName "Use generic-lens or generic-optics with 'taskRunSecurityConfigurationName' instead." #-}

instance Lude.FromJSON TransformEncryption where
  parseJSON =
    Lude.withObject
      "TransformEncryption"
      ( \x ->
          TransformEncryption'
            Lude.<$> (x Lude..:? "MlUserDataEncryption")
            Lude.<*> (x Lude..:? "TaskRunSecurityConfigurationName")
      )

instance Lude.ToJSON TransformEncryption where
  toJSON TransformEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MlUserDataEncryption" Lude..=) Lude.<$> mlUserDataEncryption,
            ("TaskRunSecurityConfigurationName" Lude..=)
              Lude.<$> taskRunSecurityConfigurationName
          ]
      )
