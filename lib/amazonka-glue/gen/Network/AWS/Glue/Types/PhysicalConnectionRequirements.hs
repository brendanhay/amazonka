{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PhysicalConnectionRequirements
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PhysicalConnectionRequirements where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the physical requirements for a connection.
--
--
--
-- /See:/ 'physicalConnectionRequirements' smart constructor.
data PhysicalConnectionRequirements = PhysicalConnectionRequirements'
  { _pcrSecurityGroupIdList ::
      !(Maybe [Text]),
    _pcrSubnetId :: !(Maybe Text),
    _pcrAvailabilityZone ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PhysicalConnectionRequirements' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcrSecurityGroupIdList' - The security group ID list used by the connection.
--
-- * 'pcrSubnetId' - The subnet ID used by the connection.
--
-- * 'pcrAvailabilityZone' - The connection's Availability Zone. This field is redundant because the specified subnet implies the Availability Zone to be used. Currently the field must be populated, but it will be deprecated in the future.
physicalConnectionRequirements ::
  PhysicalConnectionRequirements
physicalConnectionRequirements =
  PhysicalConnectionRequirements'
    { _pcrSecurityGroupIdList =
        Nothing,
      _pcrSubnetId = Nothing,
      _pcrAvailabilityZone = Nothing
    }

-- | The security group ID list used by the connection.
pcrSecurityGroupIdList :: Lens' PhysicalConnectionRequirements [Text]
pcrSecurityGroupIdList = lens _pcrSecurityGroupIdList (\s a -> s {_pcrSecurityGroupIdList = a}) . _Default . _Coerce

-- | The subnet ID used by the connection.
pcrSubnetId :: Lens' PhysicalConnectionRequirements (Maybe Text)
pcrSubnetId = lens _pcrSubnetId (\s a -> s {_pcrSubnetId = a})

-- | The connection's Availability Zone. This field is redundant because the specified subnet implies the Availability Zone to be used. Currently the field must be populated, but it will be deprecated in the future.
pcrAvailabilityZone :: Lens' PhysicalConnectionRequirements (Maybe Text)
pcrAvailabilityZone = lens _pcrAvailabilityZone (\s a -> s {_pcrAvailabilityZone = a})

instance FromJSON PhysicalConnectionRequirements where
  parseJSON =
    withObject
      "PhysicalConnectionRequirements"
      ( \x ->
          PhysicalConnectionRequirements'
            <$> (x .:? "SecurityGroupIdList" .!= mempty)
            <*> (x .:? "SubnetId")
            <*> (x .:? "AvailabilityZone")
      )

instance Hashable PhysicalConnectionRequirements

instance NFData PhysicalConnectionRequirements

instance ToJSON PhysicalConnectionRequirements where
  toJSON PhysicalConnectionRequirements' {..} =
    object
      ( catMaybes
          [ ("SecurityGroupIdList" .=) <$> _pcrSecurityGroupIdList,
            ("SubnetId" .=) <$> _pcrSubnetId,
            ("AvailabilityZone" .=) <$> _pcrAvailabilityZone
          ]
      )
