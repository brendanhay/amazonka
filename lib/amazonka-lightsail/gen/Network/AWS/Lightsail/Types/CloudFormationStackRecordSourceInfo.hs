{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
  ( CloudFormationStackRecordSourceInfo (..),

    -- * Smart constructor
    mkCloudFormationStackRecordSourceInfo,

    -- * Lenses
    cfsrsiArn,
    cfsrsiName,
    cfsrsiResourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the source of a CloudFormation stack record (i.e., the export snapshot record).
--
-- /See:/ 'mkCloudFormationStackRecordSourceInfo' smart constructor.
data CloudFormationStackRecordSourceInfo = CloudFormationStackRecordSourceInfo'
  { -- | The Amazon Resource Name (ARN) of the export snapshot record.
    arn :: Core.Maybe Types.NonEmptyString,
    -- | The name of the record.
    name :: Core.Maybe Types.NonEmptyString,
    -- | The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
    resourceType :: Core.Maybe Types.CloudFormationStackRecordSourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudFormationStackRecordSourceInfo' value with any optional fields omitted.
mkCloudFormationStackRecordSourceInfo ::
  CloudFormationStackRecordSourceInfo
mkCloudFormationStackRecordSourceInfo =
  CloudFormationStackRecordSourceInfo'
    { arn = Core.Nothing,
      name = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the export snapshot record.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrsiArn :: Lens.Lens' CloudFormationStackRecordSourceInfo (Core.Maybe Types.NonEmptyString)
cfsrsiArn = Lens.field @"arn"
{-# DEPRECATED cfsrsiArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the record.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrsiName :: Lens.Lens' CloudFormationStackRecordSourceInfo (Core.Maybe Types.NonEmptyString)
cfsrsiName = Lens.field @"name"
{-# DEPRECATED cfsrsiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrsiResourceType :: Lens.Lens' CloudFormationStackRecordSourceInfo (Core.Maybe Types.CloudFormationStackRecordSourceType)
cfsrsiResourceType = Lens.field @"resourceType"
{-# DEPRECATED cfsrsiResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON CloudFormationStackRecordSourceInfo where
  parseJSON =
    Core.withObject "CloudFormationStackRecordSourceInfo" Core.$
      \x ->
        CloudFormationStackRecordSourceInfo'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "resourceType")
