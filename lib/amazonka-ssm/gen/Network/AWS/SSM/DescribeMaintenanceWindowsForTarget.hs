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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowsForTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the maintenance window targets or tasks that an instance is associated with.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowsForTarget
  ( -- * Creating a Request
    describeMaintenanceWindowsForTarget,
    DescribeMaintenanceWindowsForTarget,

    -- * Request Lenses
    dmwftNextToken,
    dmwftMaxResults,
    dmwftTargets,
    dmwftResourceType,

    -- * Destructuring the Response
    describeMaintenanceWindowsForTargetResponse,
    DescribeMaintenanceWindowsForTargetResponse,

    -- * Response Lenses
    dmwftrsWindowIdentities,
    dmwftrsNextToken,
    dmwftrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'describeMaintenanceWindowsForTarget' smart constructor.
data DescribeMaintenanceWindowsForTarget = DescribeMaintenanceWindowsForTarget'
  { _dmwftNextToken ::
      !(Maybe Text),
    _dmwftMaxResults ::
      !(Maybe Nat),
    _dmwftTargets ::
      ![Target],
    _dmwftResourceType ::
      !MaintenanceWindowResourceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMaintenanceWindowsForTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwftNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dmwftMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'dmwftTargets' - The instance ID or key/value pair to retrieve information about.
--
-- * 'dmwftResourceType' - The type of resource you want to retrieve information about. For example, "INSTANCE".
describeMaintenanceWindowsForTarget ::
  -- | 'dmwftResourceType'
  MaintenanceWindowResourceType ->
  DescribeMaintenanceWindowsForTarget
describeMaintenanceWindowsForTarget pResourceType_ =
  DescribeMaintenanceWindowsForTarget'
    { _dmwftNextToken = Nothing,
      _dmwftMaxResults = Nothing,
      _dmwftTargets = mempty,
      _dmwftResourceType = pResourceType_
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
dmwftNextToken :: Lens' DescribeMaintenanceWindowsForTarget (Maybe Text)
dmwftNextToken = lens _dmwftNextToken (\s a -> s {_dmwftNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
dmwftMaxResults :: Lens' DescribeMaintenanceWindowsForTarget (Maybe Natural)
dmwftMaxResults = lens _dmwftMaxResults (\s a -> s {_dmwftMaxResults = a}) . mapping _Nat

-- | The instance ID or key/value pair to retrieve information about.
dmwftTargets :: Lens' DescribeMaintenanceWindowsForTarget [Target]
dmwftTargets = lens _dmwftTargets (\s a -> s {_dmwftTargets = a}) . _Coerce

-- | The type of resource you want to retrieve information about. For example, "INSTANCE".
dmwftResourceType :: Lens' DescribeMaintenanceWindowsForTarget MaintenanceWindowResourceType
dmwftResourceType = lens _dmwftResourceType (\s a -> s {_dmwftResourceType = a})

instance AWSPager DescribeMaintenanceWindowsForTarget where
  page rq rs
    | stop (rs ^. dmwftrsNextToken) = Nothing
    | stop (rs ^. dmwftrsWindowIdentities) = Nothing
    | otherwise = Just $ rq & dmwftNextToken .~ rs ^. dmwftrsNextToken

instance AWSRequest DescribeMaintenanceWindowsForTarget where
  type
    Rs DescribeMaintenanceWindowsForTarget =
      DescribeMaintenanceWindowsForTargetResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowsForTargetResponse'
            <$> (x .?> "WindowIdentities" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeMaintenanceWindowsForTarget

instance NFData DescribeMaintenanceWindowsForTarget

instance ToHeaders DescribeMaintenanceWindowsForTarget where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonSSM.DescribeMaintenanceWindowsForTarget" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeMaintenanceWindowsForTarget where
  toJSON DescribeMaintenanceWindowsForTarget' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _dmwftNextToken,
            ("MaxResults" .=) <$> _dmwftMaxResults,
            Just ("Targets" .= _dmwftTargets),
            Just ("ResourceType" .= _dmwftResourceType)
          ]
      )

instance ToPath DescribeMaintenanceWindowsForTarget where
  toPath = const "/"

instance ToQuery DescribeMaintenanceWindowsForTarget where
  toQuery = const mempty

-- | /See:/ 'describeMaintenanceWindowsForTargetResponse' smart constructor.
data DescribeMaintenanceWindowsForTargetResponse = DescribeMaintenanceWindowsForTargetResponse'
  { _dmwftrsWindowIdentities ::
      !( Maybe
           [MaintenanceWindowIdentityForTarget]
       ),
    _dmwftrsNextToken ::
      !( Maybe
           Text
       ),
    _dmwftrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeMaintenanceWindowsForTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwftrsWindowIdentities' - Information about the maintenance window targets and tasks an instance is associated with.
--
-- * 'dmwftrsNextToken' - The token for the next set of items to return. (You use this token in the next call.)
--
-- * 'dmwftrsResponseStatus' - -- | The response status code.
describeMaintenanceWindowsForTargetResponse ::
  -- | 'dmwftrsResponseStatus'
  Int ->
  DescribeMaintenanceWindowsForTargetResponse
describeMaintenanceWindowsForTargetResponse pResponseStatus_ =
  DescribeMaintenanceWindowsForTargetResponse'
    { _dmwftrsWindowIdentities =
        Nothing,
      _dmwftrsNextToken = Nothing,
      _dmwftrsResponseStatus = pResponseStatus_
    }

-- | Information about the maintenance window targets and tasks an instance is associated with.
dmwftrsWindowIdentities :: Lens' DescribeMaintenanceWindowsForTargetResponse [MaintenanceWindowIdentityForTarget]
dmwftrsWindowIdentities = lens _dmwftrsWindowIdentities (\s a -> s {_dmwftrsWindowIdentities = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You use this token in the next call.)
dmwftrsNextToken :: Lens' DescribeMaintenanceWindowsForTargetResponse (Maybe Text)
dmwftrsNextToken = lens _dmwftrsNextToken (\s a -> s {_dmwftrsNextToken = a})

-- | -- | The response status code.
dmwftrsResponseStatus :: Lens' DescribeMaintenanceWindowsForTargetResponse Int
dmwftrsResponseStatus = lens _dmwftrsResponseStatus (\s a -> s {_dmwftrsResponseStatus = a})

instance NFData DescribeMaintenanceWindowsForTargetResponse
