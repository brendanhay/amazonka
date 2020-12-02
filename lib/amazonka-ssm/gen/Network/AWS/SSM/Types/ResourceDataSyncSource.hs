{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.ResourceDataSyncAWSOrganizationsSource

-- | Information about the source of the data included in the resource data sync.
--
--
--
-- /See:/ 'resourceDataSyncSource' smart constructor.
data ResourceDataSyncSource = ResourceDataSyncSource'
  { _rdssIncludeFutureRegions ::
      !(Maybe Bool),
    _rdssAWSOrganizationsSource ::
      !( Maybe
           ResourceDataSyncAWSOrganizationsSource
       ),
    _rdssSourceType :: !Text,
    _rdssSourceRegions :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceDataSyncSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdssIncludeFutureRegions' - Whether to automatically synchronize and aggregate data from new AWS Regions when those Regions come online.
--
-- * 'rdssAWSOrganizationsSource' - Information about the AwsOrganizationsSource resource data sync source. A sync source of this type can synchronize data from AWS Organizations.
--
-- * 'rdssSourceType' - The type of data source for the resource data sync. @SourceType@ is either @AwsOrganizations@ (if an organization is present in AWS Organizations) or @singleAccountMultiRegions@ .
--
-- * 'rdssSourceRegions' - The @SyncSource@ AWS Regions included in the resource data sync.
resourceDataSyncSource ::
  -- | 'rdssSourceType'
  Text ->
  ResourceDataSyncSource
resourceDataSyncSource pSourceType_ =
  ResourceDataSyncSource'
    { _rdssIncludeFutureRegions = Nothing,
      _rdssAWSOrganizationsSource = Nothing,
      _rdssSourceType = pSourceType_,
      _rdssSourceRegions = mempty
    }

-- | Whether to automatically synchronize and aggregate data from new AWS Regions when those Regions come online.
rdssIncludeFutureRegions :: Lens' ResourceDataSyncSource (Maybe Bool)
rdssIncludeFutureRegions = lens _rdssIncludeFutureRegions (\s a -> s {_rdssIncludeFutureRegions = a})

-- | Information about the AwsOrganizationsSource resource data sync source. A sync source of this type can synchronize data from AWS Organizations.
rdssAWSOrganizationsSource :: Lens' ResourceDataSyncSource (Maybe ResourceDataSyncAWSOrganizationsSource)
rdssAWSOrganizationsSource = lens _rdssAWSOrganizationsSource (\s a -> s {_rdssAWSOrganizationsSource = a})

-- | The type of data source for the resource data sync. @SourceType@ is either @AwsOrganizations@ (if an organization is present in AWS Organizations) or @singleAccountMultiRegions@ .
rdssSourceType :: Lens' ResourceDataSyncSource Text
rdssSourceType = lens _rdssSourceType (\s a -> s {_rdssSourceType = a})

-- | The @SyncSource@ AWS Regions included in the resource data sync.
rdssSourceRegions :: Lens' ResourceDataSyncSource [Text]
rdssSourceRegions = lens _rdssSourceRegions (\s a -> s {_rdssSourceRegions = a}) . _Coerce

instance Hashable ResourceDataSyncSource

instance NFData ResourceDataSyncSource

instance ToJSON ResourceDataSyncSource where
  toJSON ResourceDataSyncSource' {..} =
    object
      ( catMaybes
          [ ("IncludeFutureRegions" .=) <$> _rdssIncludeFutureRegions,
            ("AwsOrganizationsSource" .=) <$> _rdssAWSOrganizationsSource,
            Just ("SourceType" .= _rdssSourceType),
            Just ("SourceRegions" .= _rdssSourceRegions)
          ]
      )
