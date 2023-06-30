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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigurationRecorder where

import Amazonka.Config.Types.RecordingGroup
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    --
    -- While the API model does not require this field, the server will reject
    -- a request without a defined roleARN for the configuration recorder.
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
--
-- While the API model does not require this field, the server will reject
-- a request without a defined roleARN for the configuration recorder.
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
--
-- While the API model does not require this field, the server will reject
-- a request without a defined roleARN for the configuration recorder.
configurationRecorder_roleARN :: Lens.Lens' ConfigurationRecorder (Prelude.Maybe Prelude.Text)
configurationRecorder_roleARN = Lens.lens (\ConfigurationRecorder' {roleARN} -> roleARN) (\s@ConfigurationRecorder' {} a -> s {roleARN = a} :: ConfigurationRecorder)

instance Data.FromJSON ConfigurationRecorder where
  parseJSON =
    Data.withObject
      "ConfigurationRecorder"
      ( \x ->
          ConfigurationRecorder'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "recordingGroup")
            Prelude.<*> (x Data..:? "roleARN")
      )

instance Prelude.Hashable ConfigurationRecorder where
  hashWithSalt _salt ConfigurationRecorder' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recordingGroup
      `Prelude.hashWithSalt` roleARN

instance Prelude.NFData ConfigurationRecorder where
  rnf ConfigurationRecorder' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf recordingGroup
      `Prelude.seq` Prelude.rnf roleARN

instance Data.ToJSON ConfigurationRecorder where
  toJSON ConfigurationRecorder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("recordingGroup" Data..=)
              Prelude.<$> recordingGroup,
            ("roleARN" Data..=) Prelude.<$> roleARN
          ]
      )
