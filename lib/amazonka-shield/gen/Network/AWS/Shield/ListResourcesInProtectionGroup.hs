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
-- Module      : Network.AWS.Shield.ListResourcesInProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resources that are included in the protection group.
module Network.AWS.Shield.ListResourcesInProtectionGroup
  ( -- * Creating a Request
    listResourcesInProtectionGroup,
    ListResourcesInProtectionGroup,

    -- * Request Lenses
    lripgNextToken,
    lripgMaxResults,
    lripgProtectionGroupId,

    -- * Destructuring the Response
    listResourcesInProtectionGroupResponse,
    ListResourcesInProtectionGroupResponse,

    -- * Response Lenses
    lripgrsNextToken,
    lripgrsResponseStatus,
    lripgrsResourceARNs,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'listResourcesInProtectionGroup' smart constructor.
data ListResourcesInProtectionGroup = ListResourcesInProtectionGroup'
  { _lripgNextToken ::
      !(Maybe Text),
    _lripgMaxResults ::
      !(Maybe Nat),
    _lripgProtectionGroupId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListResourcesInProtectionGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lripgNextToken' - The next token value from a previous call to @ListResourcesInProtectionGroup@ . Pass null if this is the first call.
--
-- * 'lripgMaxResults' - The maximum number of resource ARN objects to return. If you leave this blank, Shield Advanced returns the first 20 results. This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
--
-- * 'lripgProtectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
listResourcesInProtectionGroup ::
  -- | 'lripgProtectionGroupId'
  Text ->
  ListResourcesInProtectionGroup
listResourcesInProtectionGroup pProtectionGroupId_ =
  ListResourcesInProtectionGroup'
    { _lripgNextToken = Nothing,
      _lripgMaxResults = Nothing,
      _lripgProtectionGroupId = pProtectionGroupId_
    }

-- | The next token value from a previous call to @ListResourcesInProtectionGroup@ . Pass null if this is the first call.
lripgNextToken :: Lens' ListResourcesInProtectionGroup (Maybe Text)
lripgNextToken = lens _lripgNextToken (\s a -> s {_lripgNextToken = a})

-- | The maximum number of resource ARN objects to return. If you leave this blank, Shield Advanced returns the first 20 results. This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
lripgMaxResults :: Lens' ListResourcesInProtectionGroup (Maybe Natural)
lripgMaxResults = lens _lripgMaxResults (\s a -> s {_lripgMaxResults = a}) . mapping _Nat

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
lripgProtectionGroupId :: Lens' ListResourcesInProtectionGroup Text
lripgProtectionGroupId = lens _lripgProtectionGroupId (\s a -> s {_lripgProtectionGroupId = a})

instance AWSRequest ListResourcesInProtectionGroup where
  type
    Rs ListResourcesInProtectionGroup =
      ListResourcesInProtectionGroupResponse
  request = postJSON shield
  response =
    receiveJSON
      ( \s h x ->
          ListResourcesInProtectionGroupResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "ResourceArns" .!@ mempty)
      )

instance Hashable ListResourcesInProtectionGroup

instance NFData ListResourcesInProtectionGroup

instance ToHeaders ListResourcesInProtectionGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShield_20160616.ListResourcesInProtectionGroup" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListResourcesInProtectionGroup where
  toJSON ListResourcesInProtectionGroup' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lripgNextToken,
            ("MaxResults" .=) <$> _lripgMaxResults,
            Just ("ProtectionGroupId" .= _lripgProtectionGroupId)
          ]
      )

instance ToPath ListResourcesInProtectionGroup where
  toPath = const "/"

instance ToQuery ListResourcesInProtectionGroup where
  toQuery = const mempty

-- | /See:/ 'listResourcesInProtectionGroupResponse' smart constructor.
data ListResourcesInProtectionGroupResponse = ListResourcesInProtectionGroupResponse'
  { _lripgrsNextToken ::
      !(Maybe Text),
    _lripgrsResponseStatus ::
      !Int,
    _lripgrsResourceARNs ::
      ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListResourcesInProtectionGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lripgrsNextToken' - If you specify a value for @MaxResults@ and you have more resources in the protection group than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects.
--
-- * 'lripgrsResponseStatus' - -- | The response status code.
--
-- * 'lripgrsResourceARNs' - The Amazon Resource Names (ARNs) of the resources that are included in the protection group.
listResourcesInProtectionGroupResponse ::
  -- | 'lripgrsResponseStatus'
  Int ->
  ListResourcesInProtectionGroupResponse
listResourcesInProtectionGroupResponse pResponseStatus_ =
  ListResourcesInProtectionGroupResponse'
    { _lripgrsNextToken =
        Nothing,
      _lripgrsResponseStatus = pResponseStatus_,
      _lripgrsResourceARNs = mempty
    }

-- | If you specify a value for @MaxResults@ and you have more resources in the protection group than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects.
lripgrsNextToken :: Lens' ListResourcesInProtectionGroupResponse (Maybe Text)
lripgrsNextToken = lens _lripgrsNextToken (\s a -> s {_lripgrsNextToken = a})

-- | -- | The response status code.
lripgrsResponseStatus :: Lens' ListResourcesInProtectionGroupResponse Int
lripgrsResponseStatus = lens _lripgrsResponseStatus (\s a -> s {_lripgrsResponseStatus = a})

-- | The Amazon Resource Names (ARNs) of the resources that are included in the protection group.
lripgrsResourceARNs :: Lens' ListResourcesInProtectionGroupResponse [Text]
lripgrsResourceARNs = lens _lripgrsResourceARNs (\s a -> s {_lripgrsResourceARNs = a}) . _Coerce

instance NFData ListResourcesInProtectionGroupResponse
