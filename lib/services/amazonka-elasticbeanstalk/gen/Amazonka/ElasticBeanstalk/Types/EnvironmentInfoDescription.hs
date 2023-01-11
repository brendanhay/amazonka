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
-- Module      : Amazonka.ElasticBeanstalk.Types.EnvironmentInfoDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.EnvironmentInfoDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.EnvironmentInfoType
import qualified Amazonka.Prelude as Prelude

-- | The information retrieved from the Amazon EC2 instances.
--
-- /See:/ 'newEnvironmentInfoDescription' smart constructor.
data EnvironmentInfoDescription = EnvironmentInfoDescription'
  { -- | The Amazon EC2 Instance ID for this information.
    ec2InstanceId :: Prelude.Maybe Prelude.Text,
    -- | The type of information retrieved.
    infoType :: Prelude.Maybe EnvironmentInfoType,
    -- | The retrieved information. Currently contains a presigned Amazon S3 URL.
    -- The files are deleted after 15 minutes.
    --
    -- Anyone in possession of this URL can access the files before they are
    -- deleted. Make the URL available only to trusted parties.
    message :: Prelude.Maybe Prelude.Text,
    -- | The time stamp when this information was retrieved.
    sampleTimestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentInfoDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2InstanceId', 'environmentInfoDescription_ec2InstanceId' - The Amazon EC2 Instance ID for this information.
--
-- 'infoType', 'environmentInfoDescription_infoType' - The type of information retrieved.
--
-- 'message', 'environmentInfoDescription_message' - The retrieved information. Currently contains a presigned Amazon S3 URL.
-- The files are deleted after 15 minutes.
--
-- Anyone in possession of this URL can access the files before they are
-- deleted. Make the URL available only to trusted parties.
--
-- 'sampleTimestamp', 'environmentInfoDescription_sampleTimestamp' - The time stamp when this information was retrieved.
newEnvironmentInfoDescription ::
  EnvironmentInfoDescription
newEnvironmentInfoDescription =
  EnvironmentInfoDescription'
    { ec2InstanceId =
        Prelude.Nothing,
      infoType = Prelude.Nothing,
      message = Prelude.Nothing,
      sampleTimestamp = Prelude.Nothing
    }

-- | The Amazon EC2 Instance ID for this information.
environmentInfoDescription_ec2InstanceId :: Lens.Lens' EnvironmentInfoDescription (Prelude.Maybe Prelude.Text)
environmentInfoDescription_ec2InstanceId = Lens.lens (\EnvironmentInfoDescription' {ec2InstanceId} -> ec2InstanceId) (\s@EnvironmentInfoDescription' {} a -> s {ec2InstanceId = a} :: EnvironmentInfoDescription)

-- | The type of information retrieved.
environmentInfoDescription_infoType :: Lens.Lens' EnvironmentInfoDescription (Prelude.Maybe EnvironmentInfoType)
environmentInfoDescription_infoType = Lens.lens (\EnvironmentInfoDescription' {infoType} -> infoType) (\s@EnvironmentInfoDescription' {} a -> s {infoType = a} :: EnvironmentInfoDescription)

-- | The retrieved information. Currently contains a presigned Amazon S3 URL.
-- The files are deleted after 15 minutes.
--
-- Anyone in possession of this URL can access the files before they are
-- deleted. Make the URL available only to trusted parties.
environmentInfoDescription_message :: Lens.Lens' EnvironmentInfoDescription (Prelude.Maybe Prelude.Text)
environmentInfoDescription_message = Lens.lens (\EnvironmentInfoDescription' {message} -> message) (\s@EnvironmentInfoDescription' {} a -> s {message = a} :: EnvironmentInfoDescription)

-- | The time stamp when this information was retrieved.
environmentInfoDescription_sampleTimestamp :: Lens.Lens' EnvironmentInfoDescription (Prelude.Maybe Prelude.UTCTime)
environmentInfoDescription_sampleTimestamp = Lens.lens (\EnvironmentInfoDescription' {sampleTimestamp} -> sampleTimestamp) (\s@EnvironmentInfoDescription' {} a -> s {sampleTimestamp = a} :: EnvironmentInfoDescription) Prelude.. Lens.mapping Data._Time

instance Data.FromXML EnvironmentInfoDescription where
  parseXML x =
    EnvironmentInfoDescription'
      Prelude.<$> (x Data..@? "Ec2InstanceId")
      Prelude.<*> (x Data..@? "InfoType")
      Prelude.<*> (x Data..@? "Message")
      Prelude.<*> (x Data..@? "SampleTimestamp")

instance Prelude.Hashable EnvironmentInfoDescription where
  hashWithSalt _salt EnvironmentInfoDescription' {..} =
    _salt `Prelude.hashWithSalt` ec2InstanceId
      `Prelude.hashWithSalt` infoType
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` sampleTimestamp

instance Prelude.NFData EnvironmentInfoDescription where
  rnf EnvironmentInfoDescription' {..} =
    Prelude.rnf ec2InstanceId
      `Prelude.seq` Prelude.rnf infoType
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf sampleTimestamp
