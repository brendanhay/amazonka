{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformEncryption where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.MLUserDataEncryption
import qualified Network.AWS.Lens as Lens

-- | The encryption-at-rest settings of the transform that apply to accessing
-- user data. Machine learning transforms can access user data encrypted in
-- Amazon S3 using KMS.
--
-- Additionally, imported labels and trained transforms can now be
-- encrypted using a customer provided KMS key.
--
-- /See:/ 'newTransformEncryption' smart constructor.
data TransformEncryption = TransformEncryption'
  { -- | An @MLUserDataEncryption@ object containing the encryption mode and
    -- customer-provided KMS key ID.
    mlUserDataEncryption :: Core.Maybe MLUserDataEncryption,
    -- | The name of the security configuration.
    taskRunSecurityConfigurationName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransformEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mlUserDataEncryption', 'transformEncryption_mlUserDataEncryption' - An @MLUserDataEncryption@ object containing the encryption mode and
-- customer-provided KMS key ID.
--
-- 'taskRunSecurityConfigurationName', 'transformEncryption_taskRunSecurityConfigurationName' - The name of the security configuration.
newTransformEncryption ::
  TransformEncryption
newTransformEncryption =
  TransformEncryption'
    { mlUserDataEncryption =
        Core.Nothing,
      taskRunSecurityConfigurationName = Core.Nothing
    }

-- | An @MLUserDataEncryption@ object containing the encryption mode and
-- customer-provided KMS key ID.
transformEncryption_mlUserDataEncryption :: Lens.Lens' TransformEncryption (Core.Maybe MLUserDataEncryption)
transformEncryption_mlUserDataEncryption = Lens.lens (\TransformEncryption' {mlUserDataEncryption} -> mlUserDataEncryption) (\s@TransformEncryption' {} a -> s {mlUserDataEncryption = a} :: TransformEncryption)

-- | The name of the security configuration.
transformEncryption_taskRunSecurityConfigurationName :: Lens.Lens' TransformEncryption (Core.Maybe Core.Text)
transformEncryption_taskRunSecurityConfigurationName = Lens.lens (\TransformEncryption' {taskRunSecurityConfigurationName} -> taskRunSecurityConfigurationName) (\s@TransformEncryption' {} a -> s {taskRunSecurityConfigurationName = a} :: TransformEncryption)

instance Core.FromJSON TransformEncryption where
  parseJSON =
    Core.withObject
      "TransformEncryption"
      ( \x ->
          TransformEncryption'
            Core.<$> (x Core..:? "MlUserDataEncryption")
            Core.<*> (x Core..:? "TaskRunSecurityConfigurationName")
      )

instance Core.Hashable TransformEncryption

instance Core.NFData TransformEncryption

instance Core.ToJSON TransformEncryption where
  toJSON TransformEncryption' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MlUserDataEncryption" Core..=)
              Core.<$> mlUserDataEncryption,
            ("TaskRunSecurityConfigurationName" Core..=)
              Core.<$> taskRunSecurityConfigurationName
          ]
      )
