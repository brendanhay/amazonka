{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EFSAuthorizationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.EFSAuthorizationConfig
  ( EFSAuthorizationConfig (..),

    -- * Smart constructor
    mkEFSAuthorizationConfig,

    -- * Lenses
    efsacAccessPointId,
    efsacIam,
  )
where

import Network.AWS.ECS.Types.EFSAuthorizationConfigIAM
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The authorization configuration details for the Amazon EFS file system.
--
-- /See:/ 'mkEFSAuthorizationConfig' smart constructor.
data EFSAuthorizationConfig = EFSAuthorizationConfig'
  { accessPointId ::
      Lude.Maybe Lude.Text,
    iam :: Lude.Maybe EFSAuthorizationConfigIAM
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EFSAuthorizationConfig' with the minimum fields required to make a request.
--
-- * 'accessPointId' - The Amazon EFS access point ID to use. If an access point is specified, the root directory value specified in the @EFSVolumeConfiguration@ must either be omitted or set to @/@ which will enforce the path set on the EFS access point. If an access point is used, transit encryption must be enabled in the @EFSVolumeConfiguration@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS Access Points> in the /Amazon Elastic File System User Guide/ .
-- * 'iam' - Whether or not to use the Amazon ECS task IAM role defined in a task definition when mounting the Amazon EFS file system. If enabled, transit encryption must be enabled in the @EFSVolumeConfiguration@ . If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS Access Points> in the /Amazon Elastic Container Service Developer Guide/ .
mkEFSAuthorizationConfig ::
  EFSAuthorizationConfig
mkEFSAuthorizationConfig =
  EFSAuthorizationConfig'
    { accessPointId = Lude.Nothing,
      iam = Lude.Nothing
    }

-- | The Amazon EFS access point ID to use. If an access point is specified, the root directory value specified in the @EFSVolumeConfiguration@ must either be omitted or set to @/@ which will enforce the path set on the EFS access point. If an access point is used, transit encryption must be enabled in the @EFSVolumeConfiguration@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS Access Points> in the /Amazon Elastic File System User Guide/ .
--
-- /Note:/ Consider using 'accessPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsacAccessPointId :: Lens.Lens' EFSAuthorizationConfig (Lude.Maybe Lude.Text)
efsacAccessPointId = Lens.lens (accessPointId :: EFSAuthorizationConfig -> Lude.Maybe Lude.Text) (\s a -> s {accessPointId = a} :: EFSAuthorizationConfig)
{-# DEPRECATED efsacAccessPointId "Use generic-lens or generic-optics with 'accessPointId' instead." #-}

-- | Whether or not to use the Amazon ECS task IAM role defined in a task definition when mounting the Amazon EFS file system. If enabled, transit encryption must be enabled in the @EFSVolumeConfiguration@ . If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS Access Points> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'iam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsacIam :: Lens.Lens' EFSAuthorizationConfig (Lude.Maybe EFSAuthorizationConfigIAM)
efsacIam = Lens.lens (iam :: EFSAuthorizationConfig -> Lude.Maybe EFSAuthorizationConfigIAM) (\s a -> s {iam = a} :: EFSAuthorizationConfig)
{-# DEPRECATED efsacIam "Use generic-lens or generic-optics with 'iam' instead." #-}

instance Lude.FromJSON EFSAuthorizationConfig where
  parseJSON =
    Lude.withObject
      "EFSAuthorizationConfig"
      ( \x ->
          EFSAuthorizationConfig'
            Lude.<$> (x Lude..:? "accessPointId") Lude.<*> (x Lude..:? "iam")
      )

instance Lude.ToJSON EFSAuthorizationConfig where
  toJSON EFSAuthorizationConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("accessPointId" Lude..=) Lude.<$> accessPointId,
            ("iam" Lude..=) Lude.<$> iam
          ]
      )
