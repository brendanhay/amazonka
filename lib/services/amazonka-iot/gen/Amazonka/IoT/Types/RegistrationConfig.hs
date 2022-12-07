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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | The name of the provisioning template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The template body.
    templateBody :: Prelude.Maybe Prelude.Text
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
-- 'templateName', 'registrationConfig_templateName' - The name of the provisioning template.
--
-- 'roleArn', 'registrationConfig_roleArn' - The ARN of the role.
--
-- 'templateBody', 'registrationConfig_templateBody' - The template body.
newRegistrationConfig ::
  RegistrationConfig
newRegistrationConfig =
  RegistrationConfig'
    { templateName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      templateBody = Prelude.Nothing
    }

-- | The name of the provisioning template.
registrationConfig_templateName :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Text)
registrationConfig_templateName = Lens.lens (\RegistrationConfig' {templateName} -> templateName) (\s@RegistrationConfig' {} a -> s {templateName = a} :: RegistrationConfig)

-- | The ARN of the role.
registrationConfig_roleArn :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Text)
registrationConfig_roleArn = Lens.lens (\RegistrationConfig' {roleArn} -> roleArn) (\s@RegistrationConfig' {} a -> s {roleArn = a} :: RegistrationConfig)

-- | The template body.
registrationConfig_templateBody :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Text)
registrationConfig_templateBody = Lens.lens (\RegistrationConfig' {templateBody} -> templateBody) (\s@RegistrationConfig' {} a -> s {templateBody = a} :: RegistrationConfig)

instance Data.FromJSON RegistrationConfig where
  parseJSON =
    Data.withObject
      "RegistrationConfig"
      ( \x ->
          RegistrationConfig'
            Prelude.<$> (x Data..:? "templateName")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "templateBody")
      )

instance Prelude.Hashable RegistrationConfig where
  hashWithSalt _salt RegistrationConfig' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` templateBody

instance Prelude.NFData RegistrationConfig where
  rnf RegistrationConfig' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf templateBody

instance Data.ToJSON RegistrationConfig where
  toJSON RegistrationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("templateName" Data..=) Prelude.<$> templateName,
            ("roleArn" Data..=) Prelude.<$> roleArn,
            ("templateBody" Data..=) Prelude.<$> templateBody
          ]
      )
