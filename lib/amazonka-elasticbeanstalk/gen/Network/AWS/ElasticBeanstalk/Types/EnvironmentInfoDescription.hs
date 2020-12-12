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
    eidSampleTimestamp,
    eidEC2InstanceId,
    eidInfoType,
    eidMessage,
  )
where

import Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The information retrieved from the Amazon EC2 instances.
--
-- /See:/ 'mkEnvironmentInfoDescription' smart constructor.
data EnvironmentInfoDescription = EnvironmentInfoDescription'
  { sampleTimestamp ::
      Lude.Maybe Lude.DateTime,
    ec2InstanceId :: Lude.Maybe Lude.Text,
    infoType ::
      Lude.Maybe EnvironmentInfoType,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentInfoDescription' with the minimum fields required to make a request.
--
-- * 'ec2InstanceId' - The Amazon EC2 Instance ID for this information.
-- * 'infoType' - The type of information retrieved.
-- * 'message' - The retrieved information. Currently contains a presigned Amazon S3 URL. The files are deleted after 15 minutes.
--
-- Anyone in possession of this URL can access the files before they are deleted. Make the URL available only to trusted parties.
-- * 'sampleTimestamp' - The time stamp when this information was retrieved.
mkEnvironmentInfoDescription ::
  EnvironmentInfoDescription
mkEnvironmentInfoDescription =
  EnvironmentInfoDescription'
    { sampleTimestamp = Lude.Nothing,
      ec2InstanceId = Lude.Nothing,
      infoType = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The time stamp when this information was retrieved.
--
-- /Note:/ Consider using 'sampleTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidSampleTimestamp :: Lens.Lens' EnvironmentInfoDescription (Lude.Maybe Lude.DateTime)
eidSampleTimestamp = Lens.lens (sampleTimestamp :: EnvironmentInfoDescription -> Lude.Maybe Lude.DateTime) (\s a -> s {sampleTimestamp = a} :: EnvironmentInfoDescription)
{-# DEPRECATED eidSampleTimestamp "Use generic-lens or generic-optics with 'sampleTimestamp' instead." #-}

-- | The Amazon EC2 Instance ID for this information.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidEC2InstanceId :: Lens.Lens' EnvironmentInfoDescription (Lude.Maybe Lude.Text)
eidEC2InstanceId = Lens.lens (ec2InstanceId :: EnvironmentInfoDescription -> Lude.Maybe Lude.Text) (\s a -> s {ec2InstanceId = a} :: EnvironmentInfoDescription)
{-# DEPRECATED eidEC2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | The type of information retrieved.
--
-- /Note:/ Consider using 'infoType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidInfoType :: Lens.Lens' EnvironmentInfoDescription (Lude.Maybe EnvironmentInfoType)
eidInfoType = Lens.lens (infoType :: EnvironmentInfoDescription -> Lude.Maybe EnvironmentInfoType) (\s a -> s {infoType = a} :: EnvironmentInfoDescription)
{-# DEPRECATED eidInfoType "Use generic-lens or generic-optics with 'infoType' instead." #-}

-- | The retrieved information. Currently contains a presigned Amazon S3 URL. The files are deleted after 15 minutes.
--
-- Anyone in possession of this URL can access the files before they are deleted. Make the URL available only to trusted parties.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eidMessage :: Lens.Lens' EnvironmentInfoDescription (Lude.Maybe Lude.Text)
eidMessage = Lens.lens (message :: EnvironmentInfoDescription -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: EnvironmentInfoDescription)
{-# DEPRECATED eidMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML EnvironmentInfoDescription where
  parseXML x =
    EnvironmentInfoDescription'
      Lude.<$> (x Lude..@? "SampleTimestamp")
      Lude.<*> (x Lude..@? "Ec2InstanceId")
      Lude.<*> (x Lude..@? "InfoType")
      Lude.<*> (x Lude..@? "Message")
