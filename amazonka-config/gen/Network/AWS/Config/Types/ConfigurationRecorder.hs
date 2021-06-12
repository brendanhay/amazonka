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
-- Module      : Network.AWS.Config.Types.ConfigurationRecorder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationRecorder where

import Network.AWS.Config.Types.RecordingGroup
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object that represents the recording of configuration changes of an
-- AWS resource.
--
-- /See:/ 'newConfigurationRecorder' smart constructor.
data ConfigurationRecorder = ConfigurationRecorder'
  { -- | Amazon Resource Name (ARN) of the IAM role used to describe the AWS
    -- resources associated with the account.
    roleARN :: Core.Maybe Core.Text,
    -- | The name of the recorder. By default, AWS Config automatically assigns
    -- the name \"default\" when creating the configuration recorder. You
    -- cannot change the assigned name.
    name :: Core.Maybe Core.Text,
    -- | Specifies the types of AWS resources for which AWS Config records
    -- configuration changes.
    recordingGroup :: Core.Maybe RecordingGroup
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfigurationRecorder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'configurationRecorder_roleARN' - Amazon Resource Name (ARN) of the IAM role used to describe the AWS
-- resources associated with the account.
--
-- 'name', 'configurationRecorder_name' - The name of the recorder. By default, AWS Config automatically assigns
-- the name \"default\" when creating the configuration recorder. You
-- cannot change the assigned name.
--
-- 'recordingGroup', 'configurationRecorder_recordingGroup' - Specifies the types of AWS resources for which AWS Config records
-- configuration changes.
newConfigurationRecorder ::
  ConfigurationRecorder
newConfigurationRecorder =
  ConfigurationRecorder'
    { roleARN = Core.Nothing,
      name = Core.Nothing,
      recordingGroup = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the IAM role used to describe the AWS
-- resources associated with the account.
configurationRecorder_roleARN :: Lens.Lens' ConfigurationRecorder (Core.Maybe Core.Text)
configurationRecorder_roleARN = Lens.lens (\ConfigurationRecorder' {roleARN} -> roleARN) (\s@ConfigurationRecorder' {} a -> s {roleARN = a} :: ConfigurationRecorder)

-- | The name of the recorder. By default, AWS Config automatically assigns
-- the name \"default\" when creating the configuration recorder. You
-- cannot change the assigned name.
configurationRecorder_name :: Lens.Lens' ConfigurationRecorder (Core.Maybe Core.Text)
configurationRecorder_name = Lens.lens (\ConfigurationRecorder' {name} -> name) (\s@ConfigurationRecorder' {} a -> s {name = a} :: ConfigurationRecorder)

-- | Specifies the types of AWS resources for which AWS Config records
-- configuration changes.
configurationRecorder_recordingGroup :: Lens.Lens' ConfigurationRecorder (Core.Maybe RecordingGroup)
configurationRecorder_recordingGroup = Lens.lens (\ConfigurationRecorder' {recordingGroup} -> recordingGroup) (\s@ConfigurationRecorder' {} a -> s {recordingGroup = a} :: ConfigurationRecorder)

instance Core.FromJSON ConfigurationRecorder where
  parseJSON =
    Core.withObject
      "ConfigurationRecorder"
      ( \x ->
          ConfigurationRecorder'
            Core.<$> (x Core..:? "roleARN")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "recordingGroup")
      )

instance Core.Hashable ConfigurationRecorder

instance Core.NFData ConfigurationRecorder

instance Core.ToJSON ConfigurationRecorder where
  toJSON ConfigurationRecorder' {..} =
    Core.object
      ( Core.catMaybes
          [ ("roleARN" Core..=) Core.<$> roleARN,
            ("name" Core..=) Core.<$> name,
            ("recordingGroup" Core..=) Core.<$> recordingGroup
          ]
      )
