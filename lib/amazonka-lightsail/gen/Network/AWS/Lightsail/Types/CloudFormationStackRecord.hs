{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CloudFormationStackRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CloudFormationStackRecord
  ( CloudFormationStackRecord (..),

    -- * Smart constructor
    mkCloudFormationStackRecord,

    -- * Lenses
    cfsrState,
    cfsrDestinationInfo,
    cfsrResourceType,
    cfsrArn,
    cfsrCreatedAt,
    cfsrLocation,
    cfsrName,
    cfsrSourceInfo,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.RecordState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import qualified Network.AWS.Prelude as Lude

-- | Describes a CloudFormation stack record created as a result of the @create cloud formation stack@ operation.
--
-- A CloudFormation stack record provides information about the AWS CloudFormation stack used to create a new Amazon Elastic Compute Cloud instance from an exported Lightsail instance snapshot.
--
-- /See:/ 'mkCloudFormationStackRecord' smart constructor.
data CloudFormationStackRecord = CloudFormationStackRecord'
  { state ::
      Lude.Maybe RecordState,
    destinationInfo ::
      Lude.Maybe DestinationInfo,
    resourceType :: Lude.Maybe ResourceType,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    location :: Lude.Maybe ResourceLocation,
    name :: Lude.Maybe Lude.Text,
    sourceInfo ::
      Lude.Maybe
        [CloudFormationStackRecordSourceInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudFormationStackRecord' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the CloudFormation stack record.
-- * 'createdAt' - The date when the CloudFormation stack record was created.
-- * 'destinationInfo' - A list of objects describing the destination service, which is AWS CloudFormation, and the Amazon Resource Name (ARN) of the AWS CloudFormation stack.
-- * 'location' - A list of objects describing the Availability Zone and AWS Region of the CloudFormation stack record.
-- * 'name' - The name of the CloudFormation stack record. It starts with @CloudFormationStackRecord@ followed by a GUID.
-- * 'resourceType' - The Lightsail resource type (e.g., @CloudFormationStackRecord@ ).
-- * 'sourceInfo' - A list of objects describing the source of the CloudFormation stack record.
-- * 'state' - The current state of the CloudFormation stack record.
mkCloudFormationStackRecord ::
  CloudFormationStackRecord
mkCloudFormationStackRecord =
  CloudFormationStackRecord'
    { state = Lude.Nothing,
      destinationInfo = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      name = Lude.Nothing,
      sourceInfo = Lude.Nothing
    }

-- | The current state of the CloudFormation stack record.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrState :: Lens.Lens' CloudFormationStackRecord (Lude.Maybe RecordState)
cfsrState = Lens.lens (state :: CloudFormationStackRecord -> Lude.Maybe RecordState) (\s a -> s {state = a} :: CloudFormationStackRecord)
{-# DEPRECATED cfsrState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A list of objects describing the destination service, which is AWS CloudFormation, and the Amazon Resource Name (ARN) of the AWS CloudFormation stack.
--
-- /Note:/ Consider using 'destinationInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrDestinationInfo :: Lens.Lens' CloudFormationStackRecord (Lude.Maybe DestinationInfo)
cfsrDestinationInfo = Lens.lens (destinationInfo :: CloudFormationStackRecord -> Lude.Maybe DestinationInfo) (\s a -> s {destinationInfo = a} :: CloudFormationStackRecord)
{-# DEPRECATED cfsrDestinationInfo "Use generic-lens or generic-optics with 'destinationInfo' instead." #-}

-- | The Lightsail resource type (e.g., @CloudFormationStackRecord@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrResourceType :: Lens.Lens' CloudFormationStackRecord (Lude.Maybe ResourceType)
cfsrResourceType = Lens.lens (resourceType :: CloudFormationStackRecord -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: CloudFormationStackRecord)
{-# DEPRECATED cfsrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the CloudFormation stack record.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrArn :: Lens.Lens' CloudFormationStackRecord (Lude.Maybe Lude.Text)
cfsrArn = Lens.lens (arn :: CloudFormationStackRecord -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CloudFormationStackRecord)
{-# DEPRECATED cfsrArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the CloudFormation stack record was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrCreatedAt :: Lens.Lens' CloudFormationStackRecord (Lude.Maybe Lude.Timestamp)
cfsrCreatedAt = Lens.lens (createdAt :: CloudFormationStackRecord -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: CloudFormationStackRecord)
{-# DEPRECATED cfsrCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | A list of objects describing the Availability Zone and AWS Region of the CloudFormation stack record.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrLocation :: Lens.Lens' CloudFormationStackRecord (Lude.Maybe ResourceLocation)
cfsrLocation = Lens.lens (location :: CloudFormationStackRecord -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: CloudFormationStackRecord)
{-# DEPRECATED cfsrLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name of the CloudFormation stack record. It starts with @CloudFormationStackRecord@ followed by a GUID.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrName :: Lens.Lens' CloudFormationStackRecord (Lude.Maybe Lude.Text)
cfsrName = Lens.lens (name :: CloudFormationStackRecord -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CloudFormationStackRecord)
{-# DEPRECATED cfsrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of objects describing the source of the CloudFormation stack record.
--
-- /Note:/ Consider using 'sourceInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsrSourceInfo :: Lens.Lens' CloudFormationStackRecord (Lude.Maybe [CloudFormationStackRecordSourceInfo])
cfsrSourceInfo = Lens.lens (sourceInfo :: CloudFormationStackRecord -> Lude.Maybe [CloudFormationStackRecordSourceInfo]) (\s a -> s {sourceInfo = a} :: CloudFormationStackRecord)
{-# DEPRECATED cfsrSourceInfo "Use generic-lens or generic-optics with 'sourceInfo' instead." #-}

instance Lude.FromJSON CloudFormationStackRecord where
  parseJSON =
    Lude.withObject
      "CloudFormationStackRecord"
      ( \x ->
          CloudFormationStackRecord'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "destinationInfo")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "sourceInfo" Lude..!= Lude.mempty)
      )
