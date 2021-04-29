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
-- Module      : Network.AWS.Config.Types.ConfigurationRecorder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationRecorder where

import Network.AWS.Config.Types.RecordingGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents the recording of configuration changes of an
-- AWS resource.
--
-- /See:/ 'newConfigurationRecorder' smart constructor.
data ConfigurationRecorder = ConfigurationRecorder'
  { -- | Amazon Resource Name (ARN) of the IAM role used to describe the AWS
    -- resources associated with the account.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the recorder. By default, AWS Config automatically assigns
    -- the name \"default\" when creating the configuration recorder. You
    -- cannot change the assigned name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the types of AWS resources for which AWS Config records
    -- configuration changes.
    recordingGroup :: Prelude.Maybe RecordingGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { roleARN = Prelude.Nothing,
      name = Prelude.Nothing,
      recordingGroup = Prelude.Nothing
    }

-- | Amazon Resource Name (ARN) of the IAM role used to describe the AWS
-- resources associated with the account.
configurationRecorder_roleARN :: Lens.Lens' ConfigurationRecorder (Prelude.Maybe Prelude.Text)
configurationRecorder_roleARN = Lens.lens (\ConfigurationRecorder' {roleARN} -> roleARN) (\s@ConfigurationRecorder' {} a -> s {roleARN = a} :: ConfigurationRecorder)

-- | The name of the recorder. By default, AWS Config automatically assigns
-- the name \"default\" when creating the configuration recorder. You
-- cannot change the assigned name.
configurationRecorder_name :: Lens.Lens' ConfigurationRecorder (Prelude.Maybe Prelude.Text)
configurationRecorder_name = Lens.lens (\ConfigurationRecorder' {name} -> name) (\s@ConfigurationRecorder' {} a -> s {name = a} :: ConfigurationRecorder)

-- | Specifies the types of AWS resources for which AWS Config records
-- configuration changes.
configurationRecorder_recordingGroup :: Lens.Lens' ConfigurationRecorder (Prelude.Maybe RecordingGroup)
configurationRecorder_recordingGroup = Lens.lens (\ConfigurationRecorder' {recordingGroup} -> recordingGroup) (\s@ConfigurationRecorder' {} a -> s {recordingGroup = a} :: ConfigurationRecorder)

instance Prelude.FromJSON ConfigurationRecorder where
  parseJSON =
    Prelude.withObject
      "ConfigurationRecorder"
      ( \x ->
          ConfigurationRecorder'
            Prelude.<$> (x Prelude..:? "roleARN")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "recordingGroup")
      )

instance Prelude.Hashable ConfigurationRecorder

instance Prelude.NFData ConfigurationRecorder

instance Prelude.ToJSON ConfigurationRecorder where
  toJSON ConfigurationRecorder' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("roleARN" Prelude..=) Prelude.<$> roleARN,
            ("name" Prelude..=) Prelude.<$> name,
            ("recordingGroup" Prelude..=)
              Prelude.<$> recordingGroup
          ]
      )
