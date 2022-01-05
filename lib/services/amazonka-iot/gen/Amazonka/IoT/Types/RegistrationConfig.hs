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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.RegistrationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The registration configuration.
--
-- /See:/ 'newRegistrationConfig' smart constructor.
data RegistrationConfig = RegistrationConfig'
  { -- | The template body.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role.
    roleArn :: Prelude.Maybe Prelude.Text
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
-- 'templateBody', 'registrationConfig_templateBody' - The template body.
--
-- 'roleArn', 'registrationConfig_roleArn' - The ARN of the role.
newRegistrationConfig ::
  RegistrationConfig
newRegistrationConfig =
  RegistrationConfig'
    { templateBody = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The template body.
registrationConfig_templateBody :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Text)
registrationConfig_templateBody = Lens.lens (\RegistrationConfig' {templateBody} -> templateBody) (\s@RegistrationConfig' {} a -> s {templateBody = a} :: RegistrationConfig)

-- | The ARN of the role.
registrationConfig_roleArn :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Text)
registrationConfig_roleArn = Lens.lens (\RegistrationConfig' {roleArn} -> roleArn) (\s@RegistrationConfig' {} a -> s {roleArn = a} :: RegistrationConfig)

instance Core.FromJSON RegistrationConfig where
  parseJSON =
    Core.withObject
      "RegistrationConfig"
      ( \x ->
          RegistrationConfig'
            Prelude.<$> (x Core..:? "templateBody")
            Prelude.<*> (x Core..:? "roleArn")
      )

instance Prelude.Hashable RegistrationConfig where
  hashWithSalt _salt RegistrationConfig' {..} =
    _salt `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData RegistrationConfig where
  rnf RegistrationConfig' {..} =
    Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf roleArn

instance Core.ToJSON RegistrationConfig where
  toJSON RegistrationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("templateBody" Core..=) Prelude.<$> templateBody,
            ("roleArn" Core..=) Prelude.<$> roleArn
          ]
      )
