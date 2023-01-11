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
-- Module      : Amazonka.Glue.Types.TransformEncryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TransformEncryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.MLUserDataEncryption
import qualified Amazonka.Prelude as Prelude

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
    mlUserDataEncryption :: Prelude.Maybe MLUserDataEncryption,
    -- | The name of the security configuration.
    taskRunSecurityConfigurationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      taskRunSecurityConfigurationName = Prelude.Nothing
    }

-- | An @MLUserDataEncryption@ object containing the encryption mode and
-- customer-provided KMS key ID.
transformEncryption_mlUserDataEncryption :: Lens.Lens' TransformEncryption (Prelude.Maybe MLUserDataEncryption)
transformEncryption_mlUserDataEncryption = Lens.lens (\TransformEncryption' {mlUserDataEncryption} -> mlUserDataEncryption) (\s@TransformEncryption' {} a -> s {mlUserDataEncryption = a} :: TransformEncryption)

-- | The name of the security configuration.
transformEncryption_taskRunSecurityConfigurationName :: Lens.Lens' TransformEncryption (Prelude.Maybe Prelude.Text)
transformEncryption_taskRunSecurityConfigurationName = Lens.lens (\TransformEncryption' {taskRunSecurityConfigurationName} -> taskRunSecurityConfigurationName) (\s@TransformEncryption' {} a -> s {taskRunSecurityConfigurationName = a} :: TransformEncryption)

instance Data.FromJSON TransformEncryption where
  parseJSON =
    Data.withObject
      "TransformEncryption"
      ( \x ->
          TransformEncryption'
            Prelude.<$> (x Data..:? "MlUserDataEncryption")
            Prelude.<*> (x Data..:? "TaskRunSecurityConfigurationName")
      )

instance Prelude.Hashable TransformEncryption where
  hashWithSalt _salt TransformEncryption' {..} =
    _salt `Prelude.hashWithSalt` mlUserDataEncryption
      `Prelude.hashWithSalt` taskRunSecurityConfigurationName

instance Prelude.NFData TransformEncryption where
  rnf TransformEncryption' {..} =
    Prelude.rnf mlUserDataEncryption
      `Prelude.seq` Prelude.rnf taskRunSecurityConfigurationName

instance Data.ToJSON TransformEncryption where
  toJSON TransformEncryption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MlUserDataEncryption" Data..=)
              Prelude.<$> mlUserDataEncryption,
            ("TaskRunSecurityConfigurationName" Data..=)
              Prelude.<$> taskRunSecurityConfigurationName
          ]
      )
