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
-- Module      : Network.AWS.IoT.UpdateDynamicThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a dynamic thing group.
module Network.AWS.IoT.UpdateDynamicThingGroup
  ( -- * Creating a Request
    updateDynamicThingGroup,
    UpdateDynamicThingGroup,

    -- * Request Lenses
    udtgQueryVersion,
    udtgExpectedVersion,
    udtgQueryString,
    udtgIndexName,
    udtgThingGroupName,
    udtgThingGroupProperties,

    -- * Destructuring the Response
    updateDynamicThingGroupResponse,
    UpdateDynamicThingGroupResponse,

    -- * Response Lenses
    udtgrsVersion,
    udtgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDynamicThingGroup' smart constructor.
data UpdateDynamicThingGroup = UpdateDynamicThingGroup'
  { _udtgQueryVersion ::
      !(Maybe Text),
    _udtgExpectedVersion :: !(Maybe Integer),
    _udtgQueryString :: !(Maybe Text),
    _udtgIndexName :: !(Maybe Text),
    _udtgThingGroupName :: !Text,
    _udtgThingGroupProperties ::
      !ThingGroupProperties
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDynamicThingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udtgQueryVersion' - The dynamic thing group query version to update.
--
-- * 'udtgExpectedVersion' - The expected version of the dynamic thing group to update.
--
-- * 'udtgQueryString' - The dynamic thing group search query string to update.
--
-- * 'udtgIndexName' - The dynamic thing group index to update.
--
-- * 'udtgThingGroupName' - The name of the dynamic thing group to update.
--
-- * 'udtgThingGroupProperties' - The dynamic thing group properties to update.
updateDynamicThingGroup ::
  -- | 'udtgThingGroupName'
  Text ->
  -- | 'udtgThingGroupProperties'
  ThingGroupProperties ->
  UpdateDynamicThingGroup
updateDynamicThingGroup pThingGroupName_ pThingGroupProperties_ =
  UpdateDynamicThingGroup'
    { _udtgQueryVersion = Nothing,
      _udtgExpectedVersion = Nothing,
      _udtgQueryString = Nothing,
      _udtgIndexName = Nothing,
      _udtgThingGroupName = pThingGroupName_,
      _udtgThingGroupProperties = pThingGroupProperties_
    }

-- | The dynamic thing group query version to update.
udtgQueryVersion :: Lens' UpdateDynamicThingGroup (Maybe Text)
udtgQueryVersion = lens _udtgQueryVersion (\s a -> s {_udtgQueryVersion = a})

-- | The expected version of the dynamic thing group to update.
udtgExpectedVersion :: Lens' UpdateDynamicThingGroup (Maybe Integer)
udtgExpectedVersion = lens _udtgExpectedVersion (\s a -> s {_udtgExpectedVersion = a})

-- | The dynamic thing group search query string to update.
udtgQueryString :: Lens' UpdateDynamicThingGroup (Maybe Text)
udtgQueryString = lens _udtgQueryString (\s a -> s {_udtgQueryString = a})

-- | The dynamic thing group index to update.
udtgIndexName :: Lens' UpdateDynamicThingGroup (Maybe Text)
udtgIndexName = lens _udtgIndexName (\s a -> s {_udtgIndexName = a})

-- | The name of the dynamic thing group to update.
udtgThingGroupName :: Lens' UpdateDynamicThingGroup Text
udtgThingGroupName = lens _udtgThingGroupName (\s a -> s {_udtgThingGroupName = a})

-- | The dynamic thing group properties to update.
udtgThingGroupProperties :: Lens' UpdateDynamicThingGroup ThingGroupProperties
udtgThingGroupProperties = lens _udtgThingGroupProperties (\s a -> s {_udtgThingGroupProperties = a})

instance AWSRequest UpdateDynamicThingGroup where
  type Rs UpdateDynamicThingGroup = UpdateDynamicThingGroupResponse
  request = patchJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          UpdateDynamicThingGroupResponse'
            <$> (x .?> "version") <*> (pure (fromEnum s))
      )

instance Hashable UpdateDynamicThingGroup

instance NFData UpdateDynamicThingGroup

instance ToHeaders UpdateDynamicThingGroup where
  toHeaders = const mempty

instance ToJSON UpdateDynamicThingGroup where
  toJSON UpdateDynamicThingGroup' {..} =
    object
      ( catMaybes
          [ ("queryVersion" .=) <$> _udtgQueryVersion,
            ("expectedVersion" .=) <$> _udtgExpectedVersion,
            ("queryString" .=) <$> _udtgQueryString,
            ("indexName" .=) <$> _udtgIndexName,
            Just ("thingGroupProperties" .= _udtgThingGroupProperties)
          ]
      )

instance ToPath UpdateDynamicThingGroup where
  toPath UpdateDynamicThingGroup' {..} =
    mconcat ["/dynamic-thing-groups/", toBS _udtgThingGroupName]

instance ToQuery UpdateDynamicThingGroup where
  toQuery = const mempty

-- | /See:/ 'updateDynamicThingGroupResponse' smart constructor.
data UpdateDynamicThingGroupResponse = UpdateDynamicThingGroupResponse'
  { _udtgrsVersion ::
      !(Maybe Integer),
    _udtgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDynamicThingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udtgrsVersion' - The dynamic thing group version.
--
-- * 'udtgrsResponseStatus' - -- | The response status code.
updateDynamicThingGroupResponse ::
  -- | 'udtgrsResponseStatus'
  Int ->
  UpdateDynamicThingGroupResponse
updateDynamicThingGroupResponse pResponseStatus_ =
  UpdateDynamicThingGroupResponse'
    { _udtgrsVersion = Nothing,
      _udtgrsResponseStatus = pResponseStatus_
    }

-- | The dynamic thing group version.
udtgrsVersion :: Lens' UpdateDynamicThingGroupResponse (Maybe Integer)
udtgrsVersion = lens _udtgrsVersion (\s a -> s {_udtgrsVersion = a})

-- | -- | The response status code.
udtgrsResponseStatus :: Lens' UpdateDynamicThingGroupResponse Int
udtgrsResponseStatus = lens _udtgrsResponseStatus (\s a -> s {_udtgrsResponseStatus = a})

instance NFData UpdateDynamicThingGroupResponse
