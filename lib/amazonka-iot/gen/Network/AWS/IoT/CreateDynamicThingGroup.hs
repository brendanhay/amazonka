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
-- Module      : Network.AWS.IoT.CreateDynamicThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dynamic thing group.
module Network.AWS.IoT.CreateDynamicThingGroup
  ( -- * Creating a Request
    createDynamicThingGroup,
    CreateDynamicThingGroup,

    -- * Request Lenses
    cdtgQueryVersion,
    cdtgThingGroupProperties,
    cdtgIndexName,
    cdtgTags,
    cdtgThingGroupName,
    cdtgQueryString,

    -- * Destructuring the Response
    createDynamicThingGroupResponse,
    CreateDynamicThingGroupResponse,

    -- * Response Lenses
    cdtgrsQueryVersion,
    cdtgrsThingGroupARN,
    cdtgrsThingGroupId,
    cdtgrsThingGroupName,
    cdtgrsQueryString,
    cdtgrsIndexName,
    cdtgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDynamicThingGroup' smart constructor.
data CreateDynamicThingGroup = CreateDynamicThingGroup'
  { _cdtgQueryVersion ::
      !(Maybe Text),
    _cdtgThingGroupProperties ::
      !(Maybe ThingGroupProperties),
    _cdtgIndexName :: !(Maybe Text),
    _cdtgTags :: !(Maybe [Tag]),
    _cdtgThingGroupName :: !Text,
    _cdtgQueryString :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDynamicThingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdtgQueryVersion' - The dynamic thing group query version.
--
-- * 'cdtgThingGroupProperties' - The dynamic thing group properties.
--
-- * 'cdtgIndexName' - The dynamic thing group index name.
--
-- * 'cdtgTags' - Metadata which can be used to manage the dynamic thing group.
--
-- * 'cdtgThingGroupName' - The dynamic thing group name to create.
--
-- * 'cdtgQueryString' - The dynamic thing group search query string. See <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query Syntax> for information about query string syntax.
createDynamicThingGroup ::
  -- | 'cdtgThingGroupName'
  Text ->
  -- | 'cdtgQueryString'
  Text ->
  CreateDynamicThingGroup
createDynamicThingGroup pThingGroupName_ pQueryString_ =
  CreateDynamicThingGroup'
    { _cdtgQueryVersion = Nothing,
      _cdtgThingGroupProperties = Nothing,
      _cdtgIndexName = Nothing,
      _cdtgTags = Nothing,
      _cdtgThingGroupName = pThingGroupName_,
      _cdtgQueryString = pQueryString_
    }

-- | The dynamic thing group query version.
cdtgQueryVersion :: Lens' CreateDynamicThingGroup (Maybe Text)
cdtgQueryVersion = lens _cdtgQueryVersion (\s a -> s {_cdtgQueryVersion = a})

-- | The dynamic thing group properties.
cdtgThingGroupProperties :: Lens' CreateDynamicThingGroup (Maybe ThingGroupProperties)
cdtgThingGroupProperties = lens _cdtgThingGroupProperties (\s a -> s {_cdtgThingGroupProperties = a})

-- | The dynamic thing group index name.
cdtgIndexName :: Lens' CreateDynamicThingGroup (Maybe Text)
cdtgIndexName = lens _cdtgIndexName (\s a -> s {_cdtgIndexName = a})

-- | Metadata which can be used to manage the dynamic thing group.
cdtgTags :: Lens' CreateDynamicThingGroup [Tag]
cdtgTags = lens _cdtgTags (\s a -> s {_cdtgTags = a}) . _Default . _Coerce

-- | The dynamic thing group name to create.
cdtgThingGroupName :: Lens' CreateDynamicThingGroup Text
cdtgThingGroupName = lens _cdtgThingGroupName (\s a -> s {_cdtgThingGroupName = a})

-- | The dynamic thing group search query string. See <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query Syntax> for information about query string syntax.
cdtgQueryString :: Lens' CreateDynamicThingGroup Text
cdtgQueryString = lens _cdtgQueryString (\s a -> s {_cdtgQueryString = a})

instance AWSRequest CreateDynamicThingGroup where
  type Rs CreateDynamicThingGroup = CreateDynamicThingGroupResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreateDynamicThingGroupResponse'
            <$> (x .?> "queryVersion")
            <*> (x .?> "thingGroupArn")
            <*> (x .?> "thingGroupId")
            <*> (x .?> "thingGroupName")
            <*> (x .?> "queryString")
            <*> (x .?> "indexName")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateDynamicThingGroup

instance NFData CreateDynamicThingGroup

instance ToHeaders CreateDynamicThingGroup where
  toHeaders = const mempty

instance ToJSON CreateDynamicThingGroup where
  toJSON CreateDynamicThingGroup' {..} =
    object
      ( catMaybes
          [ ("queryVersion" .=) <$> _cdtgQueryVersion,
            ("thingGroupProperties" .=) <$> _cdtgThingGroupProperties,
            ("indexName" .=) <$> _cdtgIndexName,
            ("tags" .=) <$> _cdtgTags,
            Just ("queryString" .= _cdtgQueryString)
          ]
      )

instance ToPath CreateDynamicThingGroup where
  toPath CreateDynamicThingGroup' {..} =
    mconcat ["/dynamic-thing-groups/", toBS _cdtgThingGroupName]

instance ToQuery CreateDynamicThingGroup where
  toQuery = const mempty

-- | /See:/ 'createDynamicThingGroupResponse' smart constructor.
data CreateDynamicThingGroupResponse = CreateDynamicThingGroupResponse'
  { _cdtgrsQueryVersion ::
      !(Maybe Text),
    _cdtgrsThingGroupARN ::
      !(Maybe Text),
    _cdtgrsThingGroupId ::
      !(Maybe Text),
    _cdtgrsThingGroupName ::
      !(Maybe Text),
    _cdtgrsQueryString ::
      !(Maybe Text),
    _cdtgrsIndexName ::
      !(Maybe Text),
    _cdtgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDynamicThingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdtgrsQueryVersion' - The dynamic thing group query version.
--
-- * 'cdtgrsThingGroupARN' - The dynamic thing group ARN.
--
-- * 'cdtgrsThingGroupId' - The dynamic thing group ID.
--
-- * 'cdtgrsThingGroupName' - The dynamic thing group name.
--
-- * 'cdtgrsQueryString' - The dynamic thing group search query string.
--
-- * 'cdtgrsIndexName' - The dynamic thing group index name.
--
-- * 'cdtgrsResponseStatus' - -- | The response status code.
createDynamicThingGroupResponse ::
  -- | 'cdtgrsResponseStatus'
  Int ->
  CreateDynamicThingGroupResponse
createDynamicThingGroupResponse pResponseStatus_ =
  CreateDynamicThingGroupResponse'
    { _cdtgrsQueryVersion = Nothing,
      _cdtgrsThingGroupARN = Nothing,
      _cdtgrsThingGroupId = Nothing,
      _cdtgrsThingGroupName = Nothing,
      _cdtgrsQueryString = Nothing,
      _cdtgrsIndexName = Nothing,
      _cdtgrsResponseStatus = pResponseStatus_
    }

-- | The dynamic thing group query version.
cdtgrsQueryVersion :: Lens' CreateDynamicThingGroupResponse (Maybe Text)
cdtgrsQueryVersion = lens _cdtgrsQueryVersion (\s a -> s {_cdtgrsQueryVersion = a})

-- | The dynamic thing group ARN.
cdtgrsThingGroupARN :: Lens' CreateDynamicThingGroupResponse (Maybe Text)
cdtgrsThingGroupARN = lens _cdtgrsThingGroupARN (\s a -> s {_cdtgrsThingGroupARN = a})

-- | The dynamic thing group ID.
cdtgrsThingGroupId :: Lens' CreateDynamicThingGroupResponse (Maybe Text)
cdtgrsThingGroupId = lens _cdtgrsThingGroupId (\s a -> s {_cdtgrsThingGroupId = a})

-- | The dynamic thing group name.
cdtgrsThingGroupName :: Lens' CreateDynamicThingGroupResponse (Maybe Text)
cdtgrsThingGroupName = lens _cdtgrsThingGroupName (\s a -> s {_cdtgrsThingGroupName = a})

-- | The dynamic thing group search query string.
cdtgrsQueryString :: Lens' CreateDynamicThingGroupResponse (Maybe Text)
cdtgrsQueryString = lens _cdtgrsQueryString (\s a -> s {_cdtgrsQueryString = a})

-- | The dynamic thing group index name.
cdtgrsIndexName :: Lens' CreateDynamicThingGroupResponse (Maybe Text)
cdtgrsIndexName = lens _cdtgrsIndexName (\s a -> s {_cdtgrsIndexName = a})

-- | -- | The response status code.
cdtgrsResponseStatus :: Lens' CreateDynamicThingGroupResponse Int
cdtgrsResponseStatus = lens _cdtgrsResponseStatus (\s a -> s {_cdtgrsResponseStatus = a})

instance NFData CreateDynamicThingGroupResponse
