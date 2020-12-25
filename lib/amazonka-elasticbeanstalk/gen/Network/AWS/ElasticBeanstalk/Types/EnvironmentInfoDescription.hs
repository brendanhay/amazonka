{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoDescription
  ( EnvironmentInfoDescription (..),

    -- * Smart constructor
    mkEnvironmentInfoDescription,

    -- * Lenses
    eidEc2InstanceId,
    eidInfoType,
    eidMessage,
    eidSampleTimestamp,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.Ec2InstanceId as Types
import qualified Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoType as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The information retrieved from the Amazon EC2 instances.
--
-- /See:/ 'mkEnvironmentInfoDescription' smart constructor.
data EnvironmentInfoDescription = EnvironmentInfoDescription'
  { -- | The Amazon EC2 Instance ID for this information.
    ec2InstanceId :: Core.Maybe Types.Ec2InstanceId,
    -- | The type of information retrieved.
    infoType :: Core.Maybe Types.EnvironmentInfoType,
    -- | The retrieved information. Currently contains a presigned Amazon S3 URL. The files are deleted after 15 minutes.
    --
    -- Anyone in possession of this URL can access the files before they are deleted. Make the URL available only to trusted parties.
    message :: Core.Maybe Types.Message,
    -- | The time stamp when this information was retrieved.
    sampleTimestamp :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EnvironmentInfoDescription' value with any optional fields omitted.
mkEnvironmentInfoDescription ::
  EnvironmentInfoDescription
mkEnvironmentInfoDescription =
  EnvironmentInfoDescription'
    { ec2InstanceId = Core.Nothing,
      infoType = Core.Nothing,
      message = Core.Nothing,
      sampleTimestamp = Core.Nothing
    }

-- | The Amazon EC2 Instance ID for this information.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidEc2InstanceId :: Lens.Lens' EnvironmentInfoDescription (Core.Maybe Types.Ec2InstanceId)
eidEc2InstanceId = Lens.field @"ec2InstanceId"
{-# DEPRECATED eidEc2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | The type of information retrieved.
--
-- /Note:/ Consider using 'infoType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidInfoType :: Lens.Lens' EnvironmentInfoDescription (Core.Maybe Types.EnvironmentInfoType)
eidInfoType = Lens.field @"infoType"
{-# DEPRECATED eidInfoType "Use generic-lens or generic-optics with 'infoType' instead." #-}

-- | The retrieved information. Currently contains a presigned Amazon S3 URL. The files are deleted after 15 minutes.
--
-- Anyone in possession of this URL can access the files before they are deleted. Make the URL available only to trusted parties.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidMessage :: Lens.Lens' EnvironmentInfoDescription (Core.Maybe Types.Message)
eidMessage = Lens.field @"message"
{-# DEPRECATED eidMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time stamp when this information was retrieved.
--
-- /Note:/ Consider using 'sampleTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidSampleTimestamp :: Lens.Lens' EnvironmentInfoDescription (Core.Maybe Core.UTCTime)
eidSampleTimestamp = Lens.field @"sampleTimestamp"
{-# DEPRECATED eidSampleTimestamp "Use generic-lens or generic-optics with 'sampleTimestamp' instead." #-}

instance Core.FromXML EnvironmentInfoDescription where
  parseXML x =
    EnvironmentInfoDescription'
      Core.<$> (x Core..@? "Ec2InstanceId")
      Core.<*> (x Core..@? "InfoType")
      Core.<*> (x Core..@? "Message")
      Core.<*> (x Core..@? "SampleTimestamp")
