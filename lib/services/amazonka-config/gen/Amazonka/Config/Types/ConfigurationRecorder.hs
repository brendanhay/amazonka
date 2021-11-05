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
-- Module      : Amazonka.Config.Types.ConfigurationRecorder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigurationRecorder where

import Amazonka.Config.Types.RecordingGroup
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the recording of configuration changes of an
-- Amazon Web Services resource.
--
-- /See:/ 'newConfigurationRecorder' smart constructor.
data ConfigurationRecorder = ConfigurationRecorder'
  { -- | The name of the recorder. By default, Config automatically assigns the
    -- name \"default\" when creating the configuration recorder. You cannot
    -- change the assigned name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the types of Amazon Web Services resources for which Config
    -- records configuration changes.
    recordingGroup :: Prelude.Maybe RecordingGroup,
    -- | Amazon Resource Name (ARN) of the IAM role used to describe the Amazon
    -- Web Services resources associated with the account.
    roleARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationRecorder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'configurationRecorder_name' - The name of the recorder. By default, Config automatically assigns the
-- name \"default\" when creating the configuration recorder. You cannot
-- change the assigned name.
--
-- 'recordingGroup', 'configurationRecorder_recordingGroup' - Specifies the types of Amazon Web Services resources for which Config
-- records configuration changes.
--
-- 'roleARN', 'configurationRecorder_roleARN' - Amazon Resource Name (ARN) of the IAM role used to describe the Amazon
-- Web Services resources associated with the account.
newConfigurationRecorder ::
  ConfigurationRecorder
newConfigurationRecorder =
  ConfigurationRecorder'
    { name = Prelude.Nothing,
      recordingGroup = Prelude.Nothing,
      roleARN = Prelude.Nothing
    }

-- | The name of the recorder. By default, Config automatically assigns the
-- name \"default\" when creating the configuration recorder. You cannot
-- change the assigned name.
configurationRecorder_name :: Lens.Lens' ConfigurationRecorder (Prelude.Maybe Prelude.Text)
configurationRecorder_name = Lens.lens (\ConfigurationRecorder' {name} -> name) (\s@ConfigurationRecorder' {} a -> s {name = a} :: ConfigurationRecorder)

-- | Specifies the types of Amazon Web Services resources for which Config
-- records configuration changes.
configurationRecorder_recordingGroup :: Lens.Lens' ConfigurationRecorder (Prelude.Maybe RecordingGroup)
configurationRecorder_recordingGroup = Lens.lens (\ConfigurationRecorder' {recordingGroup} -> recordingGroup) (\s@ConfigurationRecorder' {} a -> s {recordingGroup = a} :: ConfigurationRecorder)

-- | Amazon Resource Name (ARN) of the IAM role used to describe the Amazon
-- Web Services resources associated with the account.
configurationRecorder_roleARN :: Lens.Lens' ConfigurationRecorder (Prelude.Maybe Prelude.Text)
configurationRecorder_roleARN = Lens.lens (\ConfigurationRecorder' {roleARN} -> roleARN) (\s@ConfigurationRecorder' {} a -> s {roleARN = a} :: ConfigurationRecorder)

instance Core.FromJSON ConfigurationRecorder where
  parseJSON =
    Core.withObject
      "ConfigurationRecorder"
      ( \x ->
          ConfigurationRecorder'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "recordingGroup")
            Prelude.<*> (x Core..:? "roleARN")
      )

instance Prelude.Hashable ConfigurationRecorder

instance Prelude.NFData ConfigurationRecorder

instance Core.ToJSON ConfigurationRecorder where
  toJSON ConfigurationRecorder' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("recordingGroup" Core..=)
              Prelude.<$> recordingGroup,
            ("roleARN" Core..=) Prelude.<$> roleARN
          ]
      )
