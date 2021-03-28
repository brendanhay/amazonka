{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EFSAuthorizationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.EFSAuthorizationConfig
  ( EFSAuthorizationConfig (..)
  -- * Smart constructor
  , mkEFSAuthorizationConfig
  -- * Lenses
  , efsacAccessPointId
  , efsacIam
  ) where

import qualified Network.AWS.ECS.Types.EFSAuthorizationConfigIAM as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The authorization configuration details for the Amazon EFS file system.
--
-- /See:/ 'mkEFSAuthorizationConfig' smart constructor.
data EFSAuthorizationConfig = EFSAuthorizationConfig'
  { accessPointId :: Core.Maybe Core.Text
    -- ^ The Amazon EFS access point ID to use. If an access point is specified, the root directory value specified in the @EFSVolumeConfiguration@ must either be omitted or set to @/@ which will enforce the path set on the EFS access point. If an access point is used, transit encryption must be enabled in the @EFSVolumeConfiguration@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS Access Points> in the /Amazon Elastic File System User Guide/ .
  , iam :: Core.Maybe Types.EFSAuthorizationConfigIAM
    -- ^ Whether or not to use the Amazon ECS task IAM role defined in a task definition when mounting the Amazon EFS file system. If enabled, transit encryption must be enabled in the @EFSVolumeConfiguration@ . If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS Access Points> in the /Amazon Elastic Container Service Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EFSAuthorizationConfig' value with any optional fields omitted.
mkEFSAuthorizationConfig
    :: EFSAuthorizationConfig
mkEFSAuthorizationConfig
  = EFSAuthorizationConfig'{accessPointId = Core.Nothing,
                            iam = Core.Nothing}

-- | The Amazon EFS access point ID to use. If an access point is specified, the root directory value specified in the @EFSVolumeConfiguration@ must either be omitted or set to @/@ which will enforce the path set on the EFS access point. If an access point is used, transit encryption must be enabled in the @EFSVolumeConfiguration@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS Access Points> in the /Amazon Elastic File System User Guide/ .
--
-- /Note:/ Consider using 'accessPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsacAccessPointId :: Lens.Lens' EFSAuthorizationConfig (Core.Maybe Core.Text)
efsacAccessPointId = Lens.field @"accessPointId"
{-# INLINEABLE efsacAccessPointId #-}
{-# DEPRECATED accessPointId "Use generic-lens or generic-optics with 'accessPointId' instead"  #-}

-- | Whether or not to use the Amazon ECS task IAM role defined in a task definition when mounting the Amazon EFS file system. If enabled, transit encryption must be enabled in the @EFSVolumeConfiguration@ . If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS Access Points> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'iam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsacIam :: Lens.Lens' EFSAuthorizationConfig (Core.Maybe Types.EFSAuthorizationConfigIAM)
efsacIam = Lens.field @"iam"
{-# INLINEABLE efsacIam #-}
{-# DEPRECATED iam "Use generic-lens or generic-optics with 'iam' instead"  #-}

instance Core.FromJSON EFSAuthorizationConfig where
        toJSON EFSAuthorizationConfig{..}
          = Core.object
              (Core.catMaybes
                 [("accessPointId" Core..=) Core.<$> accessPointId,
                  ("iam" Core..=) Core.<$> iam])

instance Core.FromJSON EFSAuthorizationConfig where
        parseJSON
          = Core.withObject "EFSAuthorizationConfig" Core.$
              \ x ->
                EFSAuthorizationConfig' Core.<$>
                  (x Core..:? "accessPointId") Core.<*> x Core..:? "iam"
