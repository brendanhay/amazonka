{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.RegistrationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RegistrationConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The registration configuration.
--
-- /See:/ 'newRegistrationConfig' smart constructor.
data RegistrationConfig = RegistrationConfig'
  { -- | The ARN of the role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The template body.
    templateBody :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newRegistrationConfig ::
  RegistrationConfig
newRegistrationConfig =
  RegistrationConfig'
    { roleArn = Prelude.Nothing,
      templateBody = Prelude.Nothing
    }

-- | The ARN of the role.
registrationConfig_roleArn :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Text)
registrationConfig_roleArn = Lens.lens (\RegistrationConfig' {roleArn} -> roleArn) (\s@RegistrationConfig' {} a -> s {roleArn = a} :: RegistrationConfig)

-- | The template body.
registrationConfig_templateBody :: Lens.Lens' RegistrationConfig (Prelude.Maybe Prelude.Text)
registrationConfig_templateBody = Lens.lens (\RegistrationConfig' {templateBody} -> templateBody) (\s@RegistrationConfig' {} a -> s {templateBody = a} :: RegistrationConfig)

instance Prelude.FromJSON RegistrationConfig where
  parseJSON =
    Prelude.withObject
      "RegistrationConfig"
      ( \x ->
          RegistrationConfig'
            Prelude.<$> (x Prelude..:? "roleArn")
            Prelude.<*> (x Prelude..:? "templateBody")
      )

instance Prelude.Hashable RegistrationConfig

instance Prelude.NFData RegistrationConfig

instance Prelude.ToJSON RegistrationConfig where
  toJSON RegistrationConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("roleArn" Prelude..=) Prelude.<$> roleArn,
            ("templateBody" Prelude..=)
              Prelude.<$> templateBody
          ]
      )
