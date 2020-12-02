{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryVPCSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryVPCSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains VPC information for the 'CreateDirectory' or 'CreateMicrosoftAD' operation.
--
--
--
-- /See:/ 'directoryVPCSettings' smart constructor.
data DirectoryVPCSettings = DirectoryVPCSettings'
  { _dvsVPCId ::
      !Text,
    _dvsSubnetIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DirectoryVPCSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvsVPCId' - The identifier of the VPC in which to create the directory.
--
-- * 'dvsSubnetIds' - The identifiers of the subnets for the directory servers. The two subnets must be in different Availability Zones. AWS Directory Service creates a directory server and a DNS server in each of these subnets.
directoryVPCSettings ::
  -- | 'dvsVPCId'
  Text ->
  DirectoryVPCSettings
directoryVPCSettings pVPCId_ =
  DirectoryVPCSettings'
    { _dvsVPCId = pVPCId_,
      _dvsSubnetIds = mempty
    }

-- | The identifier of the VPC in which to create the directory.
dvsVPCId :: Lens' DirectoryVPCSettings Text
dvsVPCId = lens _dvsVPCId (\s a -> s {_dvsVPCId = a})

-- | The identifiers of the subnets for the directory servers. The two subnets must be in different Availability Zones. AWS Directory Service creates a directory server and a DNS server in each of these subnets.
dvsSubnetIds :: Lens' DirectoryVPCSettings [Text]
dvsSubnetIds = lens _dvsSubnetIds (\s a -> s {_dvsSubnetIds = a}) . _Coerce

instance FromJSON DirectoryVPCSettings where
  parseJSON =
    withObject
      "DirectoryVPCSettings"
      ( \x ->
          DirectoryVPCSettings'
            <$> (x .: "VpcId") <*> (x .:? "SubnetIds" .!= mempty)
      )

instance Hashable DirectoryVPCSettings

instance NFData DirectoryVPCSettings

instance ToJSON DirectoryVPCSettings where
  toJSON DirectoryVPCSettings' {..} =
    object
      ( catMaybes
          [Just ("VpcId" .= _dvsVPCId), Just ("SubnetIds" .= _dvsSubnetIds)]
      )
