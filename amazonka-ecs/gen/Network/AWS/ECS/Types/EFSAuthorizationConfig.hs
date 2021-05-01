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
-- Module      : Network.AWS.ECS.Types.EFSAuthorizationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.EFSAuthorizationConfig where

import Network.AWS.ECS.Types.EFSAuthorizationConfigIAM
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The authorization configuration details for the Amazon EFS file system.
--
-- /See:/ 'newEFSAuthorizationConfig' smart constructor.
data EFSAuthorizationConfig = EFSAuthorizationConfig'
  { -- | The Amazon EFS access point ID to use. If an access point is specified,
    -- the root directory value specified in the @EFSVolumeConfiguration@ must
    -- either be omitted or set to @\/@ which will enforce the path set on the
    -- EFS access point. If an access point is used, transit encryption must be
    -- enabled in the @EFSVolumeConfiguration@. For more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS Access Points>
    -- in the /Amazon Elastic File System User Guide/.
    accessPointId :: Prelude.Maybe Prelude.Text,
    -- | Whether or not to use the Amazon ECS task IAM role defined in a task
    -- definition when mounting the Amazon EFS file system. If enabled, transit
    -- encryption must be enabled in the @EFSVolumeConfiguration@. If this
    -- parameter is omitted, the default value of @DISABLED@ is used. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS Access Points>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    iam :: Prelude.Maybe EFSAuthorizationConfigIAM
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EFSAuthorizationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPointId', 'eFSAuthorizationConfig_accessPointId' - The Amazon EFS access point ID to use. If an access point is specified,
-- the root directory value specified in the @EFSVolumeConfiguration@ must
-- either be omitted or set to @\/@ which will enforce the path set on the
-- EFS access point. If an access point is used, transit encryption must be
-- enabled in the @EFSVolumeConfiguration@. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS Access Points>
-- in the /Amazon Elastic File System User Guide/.
--
-- 'iam', 'eFSAuthorizationConfig_iam' - Whether or not to use the Amazon ECS task IAM role defined in a task
-- definition when mounting the Amazon EFS file system. If enabled, transit
-- encryption must be enabled in the @EFSVolumeConfiguration@. If this
-- parameter is omitted, the default value of @DISABLED@ is used. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS Access Points>
-- in the /Amazon Elastic Container Service Developer Guide/.
newEFSAuthorizationConfig ::
  EFSAuthorizationConfig
newEFSAuthorizationConfig =
  EFSAuthorizationConfig'
    { accessPointId =
        Prelude.Nothing,
      iam = Prelude.Nothing
    }

-- | The Amazon EFS access point ID to use. If an access point is specified,
-- the root directory value specified in the @EFSVolumeConfiguration@ must
-- either be omitted or set to @\/@ which will enforce the path set on the
-- EFS access point. If an access point is used, transit encryption must be
-- enabled in the @EFSVolumeConfiguration@. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS Access Points>
-- in the /Amazon Elastic File System User Guide/.
eFSAuthorizationConfig_accessPointId :: Lens.Lens' EFSAuthorizationConfig (Prelude.Maybe Prelude.Text)
eFSAuthorizationConfig_accessPointId = Lens.lens (\EFSAuthorizationConfig' {accessPointId} -> accessPointId) (\s@EFSAuthorizationConfig' {} a -> s {accessPointId = a} :: EFSAuthorizationConfig)

-- | Whether or not to use the Amazon ECS task IAM role defined in a task
-- definition when mounting the Amazon EFS file system. If enabled, transit
-- encryption must be enabled in the @EFSVolumeConfiguration@. If this
-- parameter is omitted, the default value of @DISABLED@ is used. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS Access Points>
-- in the /Amazon Elastic Container Service Developer Guide/.
eFSAuthorizationConfig_iam :: Lens.Lens' EFSAuthorizationConfig (Prelude.Maybe EFSAuthorizationConfigIAM)
eFSAuthorizationConfig_iam = Lens.lens (\EFSAuthorizationConfig' {iam} -> iam) (\s@EFSAuthorizationConfig' {} a -> s {iam = a} :: EFSAuthorizationConfig)

instance Prelude.FromJSON EFSAuthorizationConfig where
  parseJSON =
    Prelude.withObject
      "EFSAuthorizationConfig"
      ( \x ->
          EFSAuthorizationConfig'
            Prelude.<$> (x Prelude..:? "accessPointId")
            Prelude.<*> (x Prelude..:? "iam")
      )

instance Prelude.Hashable EFSAuthorizationConfig

instance Prelude.NFData EFSAuthorizationConfig

instance Prelude.ToJSON EFSAuthorizationConfig where
  toJSON EFSAuthorizationConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("accessPointId" Prelude..=)
              Prelude.<$> accessPointId,
            ("iam" Prelude..=) Prelude.<$> iam
          ]
      )
