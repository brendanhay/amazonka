{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.AvailabilityZone where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The name of an Availability Zone for use during database migration. @AvailabilityZone@ is an optional parameter to the <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationInstance.html @CreateReplicationInstance@ > operation, and it’s value relates to the AWS Region of an endpoint. For example, the availability zone of an endpoint in the us-east-1 region might be us-east-1a, us-east-1b, us-east-1c, or us-east-1d.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone' {_azName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azName' - The name of the Availability Zone.
availabilityZone ::
  AvailabilityZone
availabilityZone = AvailabilityZone' {_azName = Nothing}

-- | The name of the Availability Zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\s a -> s {_azName = a})

instance FromJSON AvailabilityZone where
  parseJSON =
    withObject
      "AvailabilityZone"
      (\x -> AvailabilityZone' <$> (x .:? "Name"))

instance Hashable AvailabilityZone

instance NFData AvailabilityZone
