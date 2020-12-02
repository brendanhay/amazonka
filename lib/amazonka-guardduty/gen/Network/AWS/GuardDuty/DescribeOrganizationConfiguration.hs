{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DescribeOrganizationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the account selected as the delegated administrator for GuardDuty.
module Network.AWS.GuardDuty.DescribeOrganizationConfiguration
  ( -- * Creating a Request
    describeOrganizationConfiguration,
    DescribeOrganizationConfiguration,

    -- * Request Lenses
    docDetectorId,

    -- * Destructuring the Response
    describeOrganizationConfigurationResponse,
    DescribeOrganizationConfigurationResponse,

    -- * Response Lenses
    docrsDataSources,
    docrsResponseStatus,
    docrsAutoEnable,
    docrsMemberAccountLimitReached,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeOrganizationConfiguration' smart constructor.
newtype DescribeOrganizationConfiguration = DescribeOrganizationConfiguration'
  { _docDetectorId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOrganizationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docDetectorId' - The ID of the detector to retrieve information about the delegated administrator from.
describeOrganizationConfiguration ::
  -- | 'docDetectorId'
  Text ->
  DescribeOrganizationConfiguration
describeOrganizationConfiguration pDetectorId_ =
  DescribeOrganizationConfiguration' {_docDetectorId = pDetectorId_}

-- | The ID of the detector to retrieve information about the delegated administrator from.
docDetectorId :: Lens' DescribeOrganizationConfiguration Text
docDetectorId = lens _docDetectorId (\s a -> s {_docDetectorId = a})

instance AWSRequest DescribeOrganizationConfiguration where
  type
    Rs DescribeOrganizationConfiguration =
      DescribeOrganizationConfigurationResponse
  request = get guardDuty
  response =
    receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigurationResponse'
            <$> (x .?> "dataSources")
            <*> (pure (fromEnum s))
            <*> (x .:> "autoEnable")
            <*> (x .:> "memberAccountLimitReached")
      )

instance Hashable DescribeOrganizationConfiguration

instance NFData DescribeOrganizationConfiguration

instance ToHeaders DescribeOrganizationConfiguration where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeOrganizationConfiguration where
  toPath DescribeOrganizationConfiguration' {..} =
    mconcat ["/detector/", toBS _docDetectorId, "/admin"]

instance ToQuery DescribeOrganizationConfiguration where
  toQuery = const mempty

-- | /See:/ 'describeOrganizationConfigurationResponse' smart constructor.
data DescribeOrganizationConfigurationResponse = DescribeOrganizationConfigurationResponse'
  { _docrsDataSources ::
      !( Maybe
           OrganizationDataSourceConfigurationsResult
       ),
    _docrsResponseStatus ::
      !Int,
    _docrsAutoEnable ::
      !Bool,
    _docrsMemberAccountLimitReached ::
      !Bool
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeOrganizationConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docrsDataSources' - An object that describes which data sources are enabled automatically for member accounts.
--
-- * 'docrsResponseStatus' - -- | The response status code.
--
-- * 'docrsAutoEnable' - Indicates whether GuardDuty is automatically enabled for accounts added to the organization.
--
-- * 'docrsMemberAccountLimitReached' - Indicates whether the maximum number of allowed member accounts are already associated with the delegated administrator master account.
describeOrganizationConfigurationResponse ::
  -- | 'docrsResponseStatus'
  Int ->
  -- | 'docrsAutoEnable'
  Bool ->
  -- | 'docrsMemberAccountLimitReached'
  Bool ->
  DescribeOrganizationConfigurationResponse
describeOrganizationConfigurationResponse
  pResponseStatus_
  pAutoEnable_
  pMemberAccountLimitReached_ =
    DescribeOrganizationConfigurationResponse'
      { _docrsDataSources =
          Nothing,
        _docrsResponseStatus = pResponseStatus_,
        _docrsAutoEnable = pAutoEnable_,
        _docrsMemberAccountLimitReached =
          pMemberAccountLimitReached_
      }

-- | An object that describes which data sources are enabled automatically for member accounts.
docrsDataSources :: Lens' DescribeOrganizationConfigurationResponse (Maybe OrganizationDataSourceConfigurationsResult)
docrsDataSources = lens _docrsDataSources (\s a -> s {_docrsDataSources = a})

-- | -- | The response status code.
docrsResponseStatus :: Lens' DescribeOrganizationConfigurationResponse Int
docrsResponseStatus = lens _docrsResponseStatus (\s a -> s {_docrsResponseStatus = a})

-- | Indicates whether GuardDuty is automatically enabled for accounts added to the organization.
docrsAutoEnable :: Lens' DescribeOrganizationConfigurationResponse Bool
docrsAutoEnable = lens _docrsAutoEnable (\s a -> s {_docrsAutoEnable = a})

-- | Indicates whether the maximum number of allowed member accounts are already associated with the delegated administrator master account.
docrsMemberAccountLimitReached :: Lens' DescribeOrganizationConfigurationResponse Bool
docrsMemberAccountLimitReached = lens _docrsMemberAccountLimitReached (\s a -> s {_docrsMemberAccountLimitReached = a})

instance NFData DescribeOrganizationConfigurationResponse
