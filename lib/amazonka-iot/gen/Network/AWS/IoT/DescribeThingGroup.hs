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
-- Module      : Network.AWS.IoT.DescribeThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a thing group.
module Network.AWS.IoT.DescribeThingGroup
  ( -- * Creating a Request
    describeThingGroup,
    DescribeThingGroup,

    -- * Request Lenses
    dtgThingGroupName,

    -- * Destructuring the Response
    describeThingGroupResponse,
    DescribeThingGroupResponse,

    -- * Response Lenses
    dtgrsStatus,
    dtgrsQueryVersion,
    dtgrsThingGroupARN,
    dtgrsThingGroupId,
    dtgrsThingGroupMetadata,
    dtgrsThingGroupName,
    dtgrsQueryString,
    dtgrsVersion,
    dtgrsThingGroupProperties,
    dtgrsIndexName,
    dtgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeThingGroup' smart constructor.
newtype DescribeThingGroup = DescribeThingGroup'
  { _dtgThingGroupName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeThingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgThingGroupName' - The name of the thing group.
describeThingGroup ::
  -- | 'dtgThingGroupName'
  Text ->
  DescribeThingGroup
describeThingGroup pThingGroupName_ =
  DescribeThingGroup' {_dtgThingGroupName = pThingGroupName_}

-- | The name of the thing group.
dtgThingGroupName :: Lens' DescribeThingGroup Text
dtgThingGroupName = lens _dtgThingGroupName (\s a -> s {_dtgThingGroupName = a})

instance AWSRequest DescribeThingGroup where
  type Rs DescribeThingGroup = DescribeThingGroupResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeThingGroupResponse'
            <$> (x .?> "status")
            <*> (x .?> "queryVersion")
            <*> (x .?> "thingGroupArn")
            <*> (x .?> "thingGroupId")
            <*> (x .?> "thingGroupMetadata")
            <*> (x .?> "thingGroupName")
            <*> (x .?> "queryString")
            <*> (x .?> "version")
            <*> (x .?> "thingGroupProperties")
            <*> (x .?> "indexName")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeThingGroup

instance NFData DescribeThingGroup

instance ToHeaders DescribeThingGroup where
  toHeaders = const mempty

instance ToPath DescribeThingGroup where
  toPath DescribeThingGroup' {..} =
    mconcat ["/thing-groups/", toBS _dtgThingGroupName]

instance ToQuery DescribeThingGroup where
  toQuery = const mempty

-- | /See:/ 'describeThingGroupResponse' smart constructor.
data DescribeThingGroupResponse = DescribeThingGroupResponse'
  { _dtgrsStatus ::
      !(Maybe DynamicGroupStatus),
    _dtgrsQueryVersion :: !(Maybe Text),
    _dtgrsThingGroupARN :: !(Maybe Text),
    _dtgrsThingGroupId :: !(Maybe Text),
    _dtgrsThingGroupMetadata ::
      !(Maybe ThingGroupMetadata),
    _dtgrsThingGroupName :: !(Maybe Text),
    _dtgrsQueryString :: !(Maybe Text),
    _dtgrsVersion :: !(Maybe Integer),
    _dtgrsThingGroupProperties ::
      !(Maybe ThingGroupProperties),
    _dtgrsIndexName :: !(Maybe Text),
    _dtgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeThingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgrsStatus' - The dynamic thing group status.
--
-- * 'dtgrsQueryVersion' - The dynamic thing group query version.
--
-- * 'dtgrsThingGroupARN' - The thing group ARN.
--
-- * 'dtgrsThingGroupId' - The thing group ID.
--
-- * 'dtgrsThingGroupMetadata' - Thing group metadata.
--
-- * 'dtgrsThingGroupName' - The name of the thing group.
--
-- * 'dtgrsQueryString' - The dynamic thing group search query string.
--
-- * 'dtgrsVersion' - The version of the thing group.
--
-- * 'dtgrsThingGroupProperties' - The thing group properties.
--
-- * 'dtgrsIndexName' - The dynamic thing group index name.
--
-- * 'dtgrsResponseStatus' - -- | The response status code.
describeThingGroupResponse ::
  -- | 'dtgrsResponseStatus'
  Int ->
  DescribeThingGroupResponse
describeThingGroupResponse pResponseStatus_ =
  DescribeThingGroupResponse'
    { _dtgrsStatus = Nothing,
      _dtgrsQueryVersion = Nothing,
      _dtgrsThingGroupARN = Nothing,
      _dtgrsThingGroupId = Nothing,
      _dtgrsThingGroupMetadata = Nothing,
      _dtgrsThingGroupName = Nothing,
      _dtgrsQueryString = Nothing,
      _dtgrsVersion = Nothing,
      _dtgrsThingGroupProperties = Nothing,
      _dtgrsIndexName = Nothing,
      _dtgrsResponseStatus = pResponseStatus_
    }

-- | The dynamic thing group status.
dtgrsStatus :: Lens' DescribeThingGroupResponse (Maybe DynamicGroupStatus)
dtgrsStatus = lens _dtgrsStatus (\s a -> s {_dtgrsStatus = a})

-- | The dynamic thing group query version.
dtgrsQueryVersion :: Lens' DescribeThingGroupResponse (Maybe Text)
dtgrsQueryVersion = lens _dtgrsQueryVersion (\s a -> s {_dtgrsQueryVersion = a})

-- | The thing group ARN.
dtgrsThingGroupARN :: Lens' DescribeThingGroupResponse (Maybe Text)
dtgrsThingGroupARN = lens _dtgrsThingGroupARN (\s a -> s {_dtgrsThingGroupARN = a})

-- | The thing group ID.
dtgrsThingGroupId :: Lens' DescribeThingGroupResponse (Maybe Text)
dtgrsThingGroupId = lens _dtgrsThingGroupId (\s a -> s {_dtgrsThingGroupId = a})

-- | Thing group metadata.
dtgrsThingGroupMetadata :: Lens' DescribeThingGroupResponse (Maybe ThingGroupMetadata)
dtgrsThingGroupMetadata = lens _dtgrsThingGroupMetadata (\s a -> s {_dtgrsThingGroupMetadata = a})

-- | The name of the thing group.
dtgrsThingGroupName :: Lens' DescribeThingGroupResponse (Maybe Text)
dtgrsThingGroupName = lens _dtgrsThingGroupName (\s a -> s {_dtgrsThingGroupName = a})

-- | The dynamic thing group search query string.
dtgrsQueryString :: Lens' DescribeThingGroupResponse (Maybe Text)
dtgrsQueryString = lens _dtgrsQueryString (\s a -> s {_dtgrsQueryString = a})

-- | The version of the thing group.
dtgrsVersion :: Lens' DescribeThingGroupResponse (Maybe Integer)
dtgrsVersion = lens _dtgrsVersion (\s a -> s {_dtgrsVersion = a})

-- | The thing group properties.
dtgrsThingGroupProperties :: Lens' DescribeThingGroupResponse (Maybe ThingGroupProperties)
dtgrsThingGroupProperties = lens _dtgrsThingGroupProperties (\s a -> s {_dtgrsThingGroupProperties = a})

-- | The dynamic thing group index name.
dtgrsIndexName :: Lens' DescribeThingGroupResponse (Maybe Text)
dtgrsIndexName = lens _dtgrsIndexName (\s a -> s {_dtgrsIndexName = a})

-- | -- | The response status code.
dtgrsResponseStatus :: Lens' DescribeThingGroupResponse Int
dtgrsResponseStatus = lens _dtgrsResponseStatus (\s a -> s {_dtgrsResponseStatus = a})

instance NFData DescribeThingGroupResponse
