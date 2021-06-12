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
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoType
import qualified Network.AWS.Lens as Lens

-- | The information retrieved from the Amazon EC2 instances.
--
-- /See:/ 'newEnvironmentInfoDescription' smart constructor.
data EnvironmentInfoDescription = EnvironmentInfoDescription'
  { -- | The retrieved information. Currently contains a presigned Amazon S3 URL.
    -- The files are deleted after 15 minutes.
    --
    -- Anyone in possession of this URL can access the files before they are
    -- deleted. Make the URL available only to trusted parties.
    message :: Core.Maybe Core.Text,
    -- | The type of information retrieved.
    infoType :: Core.Maybe EnvironmentInfoType,
    -- | The Amazon EC2 Instance ID for this information.
    ec2InstanceId :: Core.Maybe Core.Text,
    -- | The time stamp when this information was retrieved.
    sampleTimestamp :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnvironmentInfoDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'environmentInfoDescription_message' - The retrieved information. Currently contains a presigned Amazon S3 URL.
-- The files are deleted after 15 minutes.
--
-- Anyone in possession of this URL can access the files before they are
-- deleted. Make the URL available only to trusted parties.
--
-- 'infoType', 'environmentInfoDescription_infoType' - The type of information retrieved.
--
-- 'ec2InstanceId', 'environmentInfoDescription_ec2InstanceId' - The Amazon EC2 Instance ID for this information.
--
-- 'sampleTimestamp', 'environmentInfoDescription_sampleTimestamp' - The time stamp when this information was retrieved.
newEnvironmentInfoDescription ::
  EnvironmentInfoDescription
newEnvironmentInfoDescription =
  EnvironmentInfoDescription'
    { message = Core.Nothing,
      infoType = Core.Nothing,
      ec2InstanceId = Core.Nothing,
      sampleTimestamp = Core.Nothing
    }

-- | The retrieved information. Currently contains a presigned Amazon S3 URL.
-- The files are deleted after 15 minutes.
--
-- Anyone in possession of this URL can access the files before they are
-- deleted. Make the URL available only to trusted parties.
environmentInfoDescription_message :: Lens.Lens' EnvironmentInfoDescription (Core.Maybe Core.Text)
environmentInfoDescription_message = Lens.lens (\EnvironmentInfoDescription' {message} -> message) (\s@EnvironmentInfoDescription' {} a -> s {message = a} :: EnvironmentInfoDescription)

-- | The type of information retrieved.
environmentInfoDescription_infoType :: Lens.Lens' EnvironmentInfoDescription (Core.Maybe EnvironmentInfoType)
environmentInfoDescription_infoType = Lens.lens (\EnvironmentInfoDescription' {infoType} -> infoType) (\s@EnvironmentInfoDescription' {} a -> s {infoType = a} :: EnvironmentInfoDescription)

-- | The Amazon EC2 Instance ID for this information.
environmentInfoDescription_ec2InstanceId :: Lens.Lens' EnvironmentInfoDescription (Core.Maybe Core.Text)
environmentInfoDescription_ec2InstanceId = Lens.lens (\EnvironmentInfoDescription' {ec2InstanceId} -> ec2InstanceId) (\s@EnvironmentInfoDescription' {} a -> s {ec2InstanceId = a} :: EnvironmentInfoDescription)

-- | The time stamp when this information was retrieved.
environmentInfoDescription_sampleTimestamp :: Lens.Lens' EnvironmentInfoDescription (Core.Maybe Core.UTCTime)
environmentInfoDescription_sampleTimestamp = Lens.lens (\EnvironmentInfoDescription' {sampleTimestamp} -> sampleTimestamp) (\s@EnvironmentInfoDescription' {} a -> s {sampleTimestamp = a} :: EnvironmentInfoDescription) Core.. Lens.mapping Core._Time

instance Core.FromXML EnvironmentInfoDescription where
  parseXML x =
    EnvironmentInfoDescription'
      Core.<$> (x Core..@? "Message")
      Core.<*> (x Core..@? "InfoType")
      Core.<*> (x Core..@? "Ec2InstanceId")
      Core.<*> (x Core..@? "SampleTimestamp")

instance Core.Hashable EnvironmentInfoDescription

instance Core.NFData EnvironmentInfoDescription
