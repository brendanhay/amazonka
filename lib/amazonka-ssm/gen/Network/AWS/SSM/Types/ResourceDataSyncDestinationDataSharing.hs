{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Synchronize Systems Manager Inventory data from multiple AWS accounts defined in AWS Organizations to a centralized S3 bucket. Data is synchronized to individual key prefixes in the central bucket. Each key prefix represents a different AWS account ID.
--
--
--
-- /See:/ 'resourceDataSyncDestinationDataSharing' smart constructor.
newtype ResourceDataSyncDestinationDataSharing = ResourceDataSyncDestinationDataSharing'
  { _rdsddsDestinationDataSharingType ::
      Maybe Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ResourceDataSyncDestinationDataSharing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsddsDestinationDataSharingType' - The sharing data type. Only @Organization@ is supported.
resourceDataSyncDestinationDataSharing ::
  ResourceDataSyncDestinationDataSharing
resourceDataSyncDestinationDataSharing =
  ResourceDataSyncDestinationDataSharing'
    { _rdsddsDestinationDataSharingType =
        Nothing
    }

-- | The sharing data type. Only @Organization@ is supported.
rdsddsDestinationDataSharingType :: Lens' ResourceDataSyncDestinationDataSharing (Maybe Text)
rdsddsDestinationDataSharingType = lens _rdsddsDestinationDataSharingType (\s a -> s {_rdsddsDestinationDataSharingType = a})

instance FromJSON ResourceDataSyncDestinationDataSharing where
  parseJSON =
    withObject
      "ResourceDataSyncDestinationDataSharing"
      ( \x ->
          ResourceDataSyncDestinationDataSharing'
            <$> (x .:? "DestinationDataSharingType")
      )

instance Hashable ResourceDataSyncDestinationDataSharing

instance NFData ResourceDataSyncDestinationDataSharing

instance ToJSON ResourceDataSyncDestinationDataSharing where
  toJSON ResourceDataSyncDestinationDataSharing' {..} =
    object
      ( catMaybes
          [ ("DestinationDataSharingType" .=)
              <$> _rdsddsDestinationDataSharingType
          ]
      )
