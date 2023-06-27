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
-- Module      : Amazonka.IoT.Types.RegistrationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.RegistrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The registration configuration.
--
-- /See:/ 'newRegistrationConfig' smart constructor.
data RegistrationConfig = RegistrationConfig'
  { -- | The ARN of the role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The template body.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioning template.
    templateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegistrationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'registrationConfig_roleArn' - The ARN of the role.
--
-- 'templateBody', 'registrationConfig_templateBody' - The template body.
--
-- 'templateName', 'registrationConfig_templateName' - The name of the provisioning template.
newRegistrationConfig ::
  RegistrationConfig
newRegistrationConfig =
  RegistrationConfig'
    { roleArn = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      templateName = Prelude.Nothing
    }

-- | The ARN of the role.
registrationConfig_roleArn :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Text)
registrationConfig_roleArn = Lens.lens (\RegistrationConfig' {roleArn} -> roleArn) (\s@RegistrationConfig' {} a -> s {roleArn = a} :: RegistrationConfig)

-- | The template body.
registrationConfig_templateBody :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Text)
registrationConfig_templateBody = Lens.lens (\RegistrationConfig' {templateBody} -> templateBody) (\s@RegistrationConfig' {} a -> s {templateBody = a} :: RegistrationConfig)

-- | The name of the provisioning template.
registrationConfig_templateName :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Text)
registrationConfig_templateName = Lens.lens (\RegistrationConfig' {templateName} -> templateName) (\s@RegistrationConfig' {} a -> s {templateName = a} :: RegistrationConfig)

instance Data.FromJSON RegistrationConfig where
  parseJSON =
    Data.withObject
      "RegistrationConfig"
      ( \x ->
          RegistrationConfig'
            Prelude.<$> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "templateBody")
            Prelude.<*> (x Data..:? "templateName")
      )

instance Prelude.Hashable RegistrationConfig where
  hashWithSalt _salt RegistrationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData RegistrationConfig where
  rnf RegistrationConfig' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToJSON RegistrationConfig where
  toJSON RegistrationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("roleArn" Data..=) Prelude.<$> roleArn,
            ("templateBody" Data..=) Prelude.<$> templateBody,
            ("templateName" Data..=) Prelude.<$> templateName
          ]
      )
