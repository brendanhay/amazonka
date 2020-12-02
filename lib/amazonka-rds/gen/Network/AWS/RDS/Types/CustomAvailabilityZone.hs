{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.CustomAvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.CustomAvailabilityZone where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.VPNDetails

-- | A custom Availability Zone (AZ) is an on-premises AZ that is integrated with a VMware vSphere cluster.
--
--
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ >
--
--
-- /See:/ 'customAvailabilityZone' smart constructor.
data CustomAvailabilityZone = CustomAvailabilityZone'
  { _cazVPNDetails ::
      !(Maybe VPNDetails),
    _cazCustomAvailabilityZoneName ::
      !(Maybe Text),
    _cazCustomAvailabilityZoneId :: !(Maybe Text),
    _cazCustomAvailabilityZoneStatus ::
      !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomAvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cazVPNDetails' - Information about the virtual private network (VPN) between the VMware vSphere cluster and the AWS website.
--
-- * 'cazCustomAvailabilityZoneName' - The name of the custom AZ.
--
-- * 'cazCustomAvailabilityZoneId' - The identifier of the custom AZ. Amazon RDS generates a unique identifier when a custom AZ is created.
--
-- * 'cazCustomAvailabilityZoneStatus' - The status of the custom AZ.
customAvailabilityZone ::
  CustomAvailabilityZone
customAvailabilityZone =
  CustomAvailabilityZone'
    { _cazVPNDetails = Nothing,
      _cazCustomAvailabilityZoneName = Nothing,
      _cazCustomAvailabilityZoneId = Nothing,
      _cazCustomAvailabilityZoneStatus = Nothing
    }

-- | Information about the virtual private network (VPN) between the VMware vSphere cluster and the AWS website.
cazVPNDetails :: Lens' CustomAvailabilityZone (Maybe VPNDetails)
cazVPNDetails = lens _cazVPNDetails (\s a -> s {_cazVPNDetails = a})

-- | The name of the custom AZ.
cazCustomAvailabilityZoneName :: Lens' CustomAvailabilityZone (Maybe Text)
cazCustomAvailabilityZoneName = lens _cazCustomAvailabilityZoneName (\s a -> s {_cazCustomAvailabilityZoneName = a})

-- | The identifier of the custom AZ. Amazon RDS generates a unique identifier when a custom AZ is created.
cazCustomAvailabilityZoneId :: Lens' CustomAvailabilityZone (Maybe Text)
cazCustomAvailabilityZoneId = lens _cazCustomAvailabilityZoneId (\s a -> s {_cazCustomAvailabilityZoneId = a})

-- | The status of the custom AZ.
cazCustomAvailabilityZoneStatus :: Lens' CustomAvailabilityZone (Maybe Text)
cazCustomAvailabilityZoneStatus = lens _cazCustomAvailabilityZoneStatus (\s a -> s {_cazCustomAvailabilityZoneStatus = a})

instance FromXML CustomAvailabilityZone where
  parseXML x =
    CustomAvailabilityZone'
      <$> (x .@? "VpnDetails")
      <*> (x .@? "CustomAvailabilityZoneName")
      <*> (x .@? "CustomAvailabilityZoneId")
      <*> (x .@? "CustomAvailabilityZoneStatus")

instance Hashable CustomAvailabilityZone

instance NFData CustomAvailabilityZone
