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
-- Module      : Amazonka.Batch.Types.EFSAuthorizationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EFSAuthorizationConfig where

import Amazonka.Batch.Types.EFSAuthorizationConfigIAM
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The authorization configuration details for the Amazon EFS file system.
--
-- /See:/ 'newEFSAuthorizationConfig' smart constructor.
data EFSAuthorizationConfig = EFSAuthorizationConfig'
  { -- | Whether or not to use the Batch job IAM role defined in a job definition
    -- when mounting the Amazon EFS file system. If enabled, transit encryption
    -- must be enabled in the @EFSVolumeConfiguration@. If this parameter is
    -- omitted, the default value of @DISABLED@ is used. For more information,
    -- see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS access points>
    -- in the /Batch User Guide/. EFS IAM authorization requires that
    -- @TransitEncryption@ be @ENABLED@ and that a @JobRoleArn@ is specified.
    iam :: Prelude.Maybe EFSAuthorizationConfigIAM,
    -- | The Amazon EFS access point ID to use. If an access point is specified,
    -- the root directory value specified in the @EFSVolumeConfiguration@ must
    -- either be omitted or set to @\/@ which enforces the path set on the EFS
    -- access point. If an access point is used, transit encryption must be
    -- enabled in the @EFSVolumeConfiguration@. For more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS access points>
    -- in the /Amazon Elastic File System User Guide/.
    accessPointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EFSAuthorizationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iam', 'eFSAuthorizationConfig_iam' - Whether or not to use the Batch job IAM role defined in a job definition
-- when mounting the Amazon EFS file system. If enabled, transit encryption
-- must be enabled in the @EFSVolumeConfiguration@. If this parameter is
-- omitted, the default value of @DISABLED@ is used. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS access points>
-- in the /Batch User Guide/. EFS IAM authorization requires that
-- @TransitEncryption@ be @ENABLED@ and that a @JobRoleArn@ is specified.
--
-- 'accessPointId', 'eFSAuthorizationConfig_accessPointId' - The Amazon EFS access point ID to use. If an access point is specified,
-- the root directory value specified in the @EFSVolumeConfiguration@ must
-- either be omitted or set to @\/@ which enforces the path set on the EFS
-- access point. If an access point is used, transit encryption must be
-- enabled in the @EFSVolumeConfiguration@. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS access points>
-- in the /Amazon Elastic File System User Guide/.
newEFSAuthorizationConfig ::
  EFSAuthorizationConfig
newEFSAuthorizationConfig =
  EFSAuthorizationConfig'
    { iam = Prelude.Nothing,
      accessPointId = Prelude.Nothing
    }

-- | Whether or not to use the Batch job IAM role defined in a job definition
-- when mounting the Amazon EFS file system. If enabled, transit encryption
-- must be enabled in the @EFSVolumeConfiguration@. If this parameter is
-- omitted, the default value of @DISABLED@ is used. For more information,
-- see
-- <https://docs.aws.amazon.com/batch/latest/userguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS access points>
-- in the /Batch User Guide/. EFS IAM authorization requires that
-- @TransitEncryption@ be @ENABLED@ and that a @JobRoleArn@ is specified.
eFSAuthorizationConfig_iam :: Lens.Lens' EFSAuthorizationConfig (Prelude.Maybe EFSAuthorizationConfigIAM)
eFSAuthorizationConfig_iam = Lens.lens (\EFSAuthorizationConfig' {iam} -> iam) (\s@EFSAuthorizationConfig' {} a -> s {iam = a} :: EFSAuthorizationConfig)

-- | The Amazon EFS access point ID to use. If an access point is specified,
-- the root directory value specified in the @EFSVolumeConfiguration@ must
-- either be omitted or set to @\/@ which enforces the path set on the EFS
-- access point. If an access point is used, transit encryption must be
-- enabled in the @EFSVolumeConfiguration@. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS access points>
-- in the /Amazon Elastic File System User Guide/.
eFSAuthorizationConfig_accessPointId :: Lens.Lens' EFSAuthorizationConfig (Prelude.Maybe Prelude.Text)
eFSAuthorizationConfig_accessPointId = Lens.lens (\EFSAuthorizationConfig' {accessPointId} -> accessPointId) (\s@EFSAuthorizationConfig' {} a -> s {accessPointId = a} :: EFSAuthorizationConfig)

instance Core.FromJSON EFSAuthorizationConfig where
  parseJSON =
    Core.withObject
      "EFSAuthorizationConfig"
      ( \x ->
          EFSAuthorizationConfig'
            Prelude.<$> (x Core..:? "iam")
            Prelude.<*> (x Core..:? "accessPointId")
      )

instance Prelude.Hashable EFSAuthorizationConfig where
  hashWithSalt _salt EFSAuthorizationConfig' {..} =
    _salt `Prelude.hashWithSalt` iam
      `Prelude.hashWithSalt` accessPointId

instance Prelude.NFData EFSAuthorizationConfig where
  rnf EFSAuthorizationConfig' {..} =
    Prelude.rnf iam
      `Prelude.seq` Prelude.rnf accessPointId

instance Core.ToJSON EFSAuthorizationConfig where
  toJSON EFSAuthorizationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("iam" Core..=) Prelude.<$> iam,
            ("accessPointId" Core..=) Prelude.<$> accessPointId
          ]
      )
