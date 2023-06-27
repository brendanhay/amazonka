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

-- | Records configuration changes to specified resource types. For more
-- information about the configuration recorder, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/stop-start-recorder.html Managing the Configuration Recorder>
-- in the /Config Developer Guide/.
--
-- /See:/ 'newConfigurationRecorder' smart constructor.
data ConfigurationRecorder = ConfigurationRecorder'
  { -- | The name of the configuration recorder. Config automatically assigns the
    -- name of \"default\" when creating the configuration recorder.
    --
    -- You cannot change the name of the configuration recorder after it has
    -- been created. To change the configuration recorder name, you must delete
    -- it and create a new configuration recorder with a new name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies which resource types Config records for configuration changes.
    --
    -- __High Number of Config Evaluations__
    --
    -- You may notice increased activity in your account during your initial
    -- month recording with Config when compared to subsequent months. During
    -- the initial bootstrapping process, Config runs evaluations on all the
    -- resources in your account that you have selected for Config to record.
    --
    -- If you are running ephemeral workloads, you may see increased activity
    -- from Config as it records configuration changes associated with creating
    -- and deleting these temporary resources. An /ephemeral workload/ is a
    -- temporary use of computing resources that are loaded and run when
    -- needed. Examples include Amazon Elastic Compute Cloud (Amazon EC2) Spot
    -- Instances, Amazon EMR jobs, and Auto Scaling. If you want to avoid the
    -- increased activity from running ephemeral workloads, you can run these
    -- types of workloads in a separate account with Config turned off to avoid
    -- increased configuration recording and rule evaluations.
    recordingGroup :: Prelude.Maybe RecordingGroup,
    -- | Amazon Resource Name (ARN) of the IAM role assumed by Config and used by
    -- the configuration recorder.
    --
    -- While the API model does not require this field, the server will reject
    -- a request without a defined @roleARN@ for the configuration recorder.
    --
    -- __Pre-existing Config role__
    --
    -- If you have used an Amazon Web Services service that uses Config, such
    -- as Security Hub or Control Tower, and an Config role has already been
    -- created, make sure that the IAM role that you use when setting up Config
    -- keeps the same minimum permissions as the already created Config role.
    -- You must do this so that the other Amazon Web Services service continues
    -- to run as expected.
    --
    -- For example, if Control Tower has an IAM role that allows Config to read
    -- Amazon Simple Storage Service (Amazon S3) objects, make sure that the
    -- same permissions are granted within the IAM role you use when setting up
    -- Config. Otherwise, it may interfere with how Control Tower operates. For
    -- more information about IAM roles for Config, see
    -- <https://docs.aws.amazon.com/config/latest/developerguide/security-iam.html Identity and Access Management for Config>
    -- in the /Config Developer Guide/.
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
-- 'name', 'configurationRecorder_name' - The name of the configuration recorder. Config automatically assigns the
-- name of \"default\" when creating the configuration recorder.
--
-- You cannot change the name of the configuration recorder after it has
-- been created. To change the configuration recorder name, you must delete
-- it and create a new configuration recorder with a new name.
--
-- 'recordingGroup', 'configurationRecorder_recordingGroup' - Specifies which resource types Config records for configuration changes.
--
-- __High Number of Config Evaluations__
--
-- You may notice increased activity in your account during your initial
-- month recording with Config when compared to subsequent months. During
-- the initial bootstrapping process, Config runs evaluations on all the
-- resources in your account that you have selected for Config to record.
--
-- If you are running ephemeral workloads, you may see increased activity
-- from Config as it records configuration changes associated with creating
-- and deleting these temporary resources. An /ephemeral workload/ is a
-- temporary use of computing resources that are loaded and run when
-- needed. Examples include Amazon Elastic Compute Cloud (Amazon EC2) Spot
-- Instances, Amazon EMR jobs, and Auto Scaling. If you want to avoid the
-- increased activity from running ephemeral workloads, you can run these
-- types of workloads in a separate account with Config turned off to avoid
-- increased configuration recording and rule evaluations.
--
-- 'roleARN', 'configurationRecorder_roleARN' - Amazon Resource Name (ARN) of the IAM role assumed by Config and used by
-- the configuration recorder.
--
-- While the API model does not require this field, the server will reject
-- a request without a defined @roleARN@ for the configuration recorder.
--
-- __Pre-existing Config role__
--
-- If you have used an Amazon Web Services service that uses Config, such
-- as Security Hub or Control Tower, and an Config role has already been
-- created, make sure that the IAM role that you use when setting up Config
-- keeps the same minimum permissions as the already created Config role.
-- You must do this so that the other Amazon Web Services service continues
-- to run as expected.
--
-- For example, if Control Tower has an IAM role that allows Config to read
-- Amazon Simple Storage Service (Amazon S3) objects, make sure that the
-- same permissions are granted within the IAM role you use when setting up
-- Config. Otherwise, it may interfere with how Control Tower operates. For
-- more information about IAM roles for Config, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/security-iam.html Identity and Access Management for Config>
-- in the /Config Developer Guide/.
newConfigurationRecorder ::
  ConfigurationRecorder
newConfigurationRecorder =
  ConfigurationRecorder'
    { name = Prelude.Nothing,
      recordingGroup = Prelude.Nothing,
      roleARN = Prelude.Nothing
    }

-- | The name of the configuration recorder. Config automatically assigns the
-- name of \"default\" when creating the configuration recorder.
--
-- You cannot change the name of the configuration recorder after it has
-- been created. To change the configuration recorder name, you must delete
-- it and create a new configuration recorder with a new name.
configurationRecorder_name :: Lens.Lens' ConfigurationRecorder (Prelude.Maybe Prelude.Text)
configurationRecorder_name = Lens.lens (\ConfigurationRecorder' {name} -> name) (\s@ConfigurationRecorder' {} a -> s {name = a} :: ConfigurationRecorder)

-- | Specifies which resource types Config records for configuration changes.
--
-- __High Number of Config Evaluations__
--
-- You may notice increased activity in your account during your initial
-- month recording with Config when compared to subsequent months. During
-- the initial bootstrapping process, Config runs evaluations on all the
-- resources in your account that you have selected for Config to record.
--
-- If you are running ephemeral workloads, you may see increased activity
-- from Config as it records configuration changes associated with creating
-- and deleting these temporary resources. An /ephemeral workload/ is a
-- temporary use of computing resources that are loaded and run when
-- needed. Examples include Amazon Elastic Compute Cloud (Amazon EC2) Spot
-- Instances, Amazon EMR jobs, and Auto Scaling. If you want to avoid the
-- increased activity from running ephemeral workloads, you can run these
-- types of workloads in a separate account with Config turned off to avoid
-- increased configuration recording and rule evaluations.
configurationRecorder_recordingGroup :: Lens.Lens' ConfigurationRecorder (Prelude.Maybe RecordingGroup)
configurationRecorder_recordingGroup = Lens.lens (\ConfigurationRecorder' {recordingGroup} -> recordingGroup) (\s@ConfigurationRecorder' {} a -> s {recordingGroup = a} :: ConfigurationRecorder)

-- | Amazon Resource Name (ARN) of the IAM role assumed by Config and used by
-- the configuration recorder.
--
-- While the API model does not require this field, the server will reject
-- a request without a defined @roleARN@ for the configuration recorder.
--
-- __Pre-existing Config role__
--
-- If you have used an Amazon Web Services service that uses Config, such
-- as Security Hub or Control Tower, and an Config role has already been
-- created, make sure that the IAM role that you use when setting up Config
-- keeps the same minimum permissions as the already created Config role.
-- You must do this so that the other Amazon Web Services service continues
-- to run as expected.
--
-- For example, if Control Tower has an IAM role that allows Config to read
-- Amazon Simple Storage Service (Amazon S3) objects, make sure that the
-- same permissions are granted within the IAM role you use when setting up
-- Config. Otherwise, it may interfere with how Control Tower operates. For
-- more information about IAM roles for Config, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/security-iam.html Identity and Access Management for Config>
-- in the /Config Developer Guide/.
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
