{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportInstanceTaskDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportInstanceTaskDetails where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ImportInstanceVolumeDetailItem
import Network.AWS.EC2.Types.PlatformValues
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an import instance task.
--
--
--
-- /See:/ 'importInstanceTaskDetails' smart constructor.
data ImportInstanceTaskDetails = ImportInstanceTaskDetails'
  { _iitdInstanceId ::
      !(Maybe Text),
    _iitdPlatform ::
      !(Maybe PlatformValues),
    _iitdVolumes ::
      !( Maybe
           [ImportInstanceVolumeDetailItem]
       ),
    _iitdDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportInstanceTaskDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iitdInstanceId' - The ID of the instance.
--
-- * 'iitdPlatform' - The instance operating system.
--
-- * 'iitdVolumes' - The volumes.
--
-- * 'iitdDescription' - A description of the task.
importInstanceTaskDetails ::
  ImportInstanceTaskDetails
importInstanceTaskDetails =
  ImportInstanceTaskDetails'
    { _iitdInstanceId = Nothing,
      _iitdPlatform = Nothing,
      _iitdVolumes = Nothing,
      _iitdDescription = Nothing
    }

-- | The ID of the instance.
iitdInstanceId :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdInstanceId = lens _iitdInstanceId (\s a -> s {_iitdInstanceId = a})

-- | The instance operating system.
iitdPlatform :: Lens' ImportInstanceTaskDetails (Maybe PlatformValues)
iitdPlatform = lens _iitdPlatform (\s a -> s {_iitdPlatform = a})

-- | The volumes.
iitdVolumes :: Lens' ImportInstanceTaskDetails [ImportInstanceVolumeDetailItem]
iitdVolumes = lens _iitdVolumes (\s a -> s {_iitdVolumes = a}) . _Default . _Coerce

-- | A description of the task.
iitdDescription :: Lens' ImportInstanceTaskDetails (Maybe Text)
iitdDescription = lens _iitdDescription (\s a -> s {_iitdDescription = a})

instance FromXML ImportInstanceTaskDetails where
  parseXML x =
    ImportInstanceTaskDetails'
      <$> (x .@? "instanceId")
      <*> (x .@? "platform")
      <*> (x .@? "volumes" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "description")

instance Hashable ImportInstanceTaskDetails

instance NFData ImportInstanceTaskDetails
