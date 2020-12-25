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

import qualified Network.AWS.Glue.Types.MLUserDataEncryption as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
--
-- Additionally, imported labels and trained transforms can now be encrypted using a customer provided KMS key.
--
-- /See:/ 'mkTransformEncryption' smart constructor.
data TransformEncryption = TransformEncryption'
  { -- | An @MLUserDataEncryption@ object containing the encryption mode and customer-provided KMS key ID.
    mlUserDataEncryption :: Core.Maybe Types.MLUserDataEncryption,
    -- | The name of the security configuration.
    taskRunSecurityConfigurationName :: Core.Maybe Types.NameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransformEncryption' value with any optional fields omitted.
mkTransformEncryption ::
  TransformEncryption
mkTransformEncryption =
  TransformEncryption'
    { mlUserDataEncryption = Core.Nothing,
      taskRunSecurityConfigurationName = Core.Nothing
    }

-- | An @MLUserDataEncryption@ object containing the encryption mode and customer-provided KMS key ID.
--
-- /Note:/ Consider using 'mlUserDataEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teMlUserDataEncryption :: Lens.Lens' TransformEncryption (Core.Maybe Types.MLUserDataEncryption)
teMlUserDataEncryption = Lens.field @"mlUserDataEncryption"
{-# DEPRECATED teMlUserDataEncryption "Use generic-lens or generic-optics with 'mlUserDataEncryption' instead." #-}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'taskRunSecurityConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teTaskRunSecurityConfigurationName :: Lens.Lens' TransformEncryption (Core.Maybe Types.NameString)
teTaskRunSecurityConfigurationName = Lens.field @"taskRunSecurityConfigurationName"
{-# DEPRECATED teTaskRunSecurityConfigurationName "Use generic-lens or generic-optics with 'taskRunSecurityConfigurationName' instead." #-}

instance Core.FromJSON TransformEncryption where
  toJSON TransformEncryption {..} =
    Core.object
      ( Core.catMaybes
          [ ("MlUserDataEncryption" Core..=) Core.<$> mlUserDataEncryption,
            ("TaskRunSecurityConfigurationName" Core..=)
              Core.<$> taskRunSecurityConfigurationName
          ]
      )

instance Core.FromJSON TransformEncryption where
  parseJSON =
    Core.withObject "TransformEncryption" Core.$
      \x ->
        TransformEncryption'
          Core.<$> (x Core..:? "MlUserDataEncryption")
          Core.<*> (x Core..:? "TaskRunSecurityConfigurationName")
