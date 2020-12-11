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
    cfsrsiResourceType,
    cfsrsiArn,
    cfsrsiName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType
import qualified Network.AWS.Prelude as Lude

-- | Describes the source of a CloudFormation stack record (i.e., the export snapshot record).
--
-- /See:/ 'mkCloudFormationStackRecordSourceInfo' smart constructor.
data CloudFormationStackRecordSourceInfo = CloudFormationStackRecordSourceInfo'
  { resourceType ::
      Lude.Maybe
        CloudFormationStackRecordSourceType,
    arn ::
      Lude.Maybe
        Lude.Text,
    name ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudFormationStackRecordSourceInfo' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the export snapshot record.
-- * 'name' - The name of the record.
-- * 'resourceType' - The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
mkCloudFormationStackRecordSourceInfo ::
  CloudFormationStackRecordSourceInfo
mkCloudFormationStackRecordSourceInfo =
  CloudFormationStackRecordSourceInfo'
    { resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrsiResourceType :: Lens.Lens' CloudFormationStackRecordSourceInfo (Lude.Maybe CloudFormationStackRecordSourceType)
cfsrsiResourceType = Lens.lens (resourceType :: CloudFormationStackRecordSourceInfo -> Lude.Maybe CloudFormationStackRecordSourceType) (\s a -> s {resourceType = a} :: CloudFormationStackRecordSourceInfo)
{-# DEPRECATED cfsrsiResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the export snapshot record.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrsiArn :: Lens.Lens' CloudFormationStackRecordSourceInfo (Lude.Maybe Lude.Text)
cfsrsiArn = Lens.lens (arn :: CloudFormationStackRecordSourceInfo -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CloudFormationStackRecordSourceInfo)
{-# DEPRECATED cfsrsiArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the record.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrsiName :: Lens.Lens' CloudFormationStackRecordSourceInfo (Lude.Maybe Lude.Text)
cfsrsiName = Lens.lens (name :: CloudFormationStackRecordSourceInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CloudFormationStackRecordSourceInfo)
{-# DEPRECATED cfsrsiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON CloudFormationStackRecordSourceInfo where
  parseJSON =
    Lude.withObject
      "CloudFormationStackRecordSourceInfo"
      ( \x ->
          CloudFormationStackRecordSourceInfo'
            Lude.<$> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "name")
      )
