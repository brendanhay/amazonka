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
-- Module      : Network.AWS.Shield.ListProtectionGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the 'ProtectionGroup' objects for the account.
module Network.AWS.Shield.ListProtectionGroups
  ( -- * Creating a Request
    listProtectionGroups,
    ListProtectionGroups,

    -- * Request Lenses
    lpgNextToken,
    lpgMaxResults,

    -- * Destructuring the Response
    listProtectionGroupsResponse,
    ListProtectionGroupsResponse,

    -- * Response Lenses
    lpgrsNextToken,
    lpgrsResponseStatus,
    lpgrsProtectionGroups,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'listProtectionGroups' smart constructor.
data ListProtectionGroups = ListProtectionGroups'
  { _lpgNextToken ::
      !(Maybe Text),
    _lpgMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListProtectionGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpgNextToken' - The next token value from a previous call to @ListProtectionGroups@ . Pass null if this is the first call.
--
-- * 'lpgMaxResults' - The maximum number of 'ProtectionGroup' objects to return. If you leave this blank, Shield Advanced returns the first 20 results. This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
listProtectionGroups ::
  ListProtectionGroups
listProtectionGroups =
  ListProtectionGroups'
    { _lpgNextToken = Nothing,
      _lpgMaxResults = Nothing
    }

-- | The next token value from a previous call to @ListProtectionGroups@ . Pass null if this is the first call.
lpgNextToken :: Lens' ListProtectionGroups (Maybe Text)
lpgNextToken = lens _lpgNextToken (\s a -> s {_lpgNextToken = a})

-- | The maximum number of 'ProtectionGroup' objects to return. If you leave this blank, Shield Advanced returns the first 20 results. This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
lpgMaxResults :: Lens' ListProtectionGroups (Maybe Natural)
lpgMaxResults = lens _lpgMaxResults (\s a -> s {_lpgMaxResults = a}) . mapping _Nat

instance AWSRequest ListProtectionGroups where
  type Rs ListProtectionGroups = ListProtectionGroupsResponse
  request = postJSON shield
  response =
    receiveJSON
      ( \s h x ->
          ListProtectionGroupsResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "ProtectionGroups" .!@ mempty)
      )

instance Hashable ListProtectionGroups

instance NFData ListProtectionGroups

instance ToHeaders ListProtectionGroups where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.ListProtectionGroups" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListProtectionGroups where
  toJSON ListProtectionGroups' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lpgNextToken,
            ("MaxResults" .=) <$> _lpgMaxResults
          ]
      )

instance ToPath ListProtectionGroups where
  toPath = const "/"

instance ToQuery ListProtectionGroups where
  toQuery = const mempty

-- | /See:/ 'listProtectionGroupsResponse' smart constructor.
data ListProtectionGroupsResponse = ListProtectionGroupsResponse'
  { _lpgrsNextToken ::
      !(Maybe Text),
    _lpgrsResponseStatus :: !Int,
    _lpgrsProtectionGroups ::
      ![ProtectionGroup]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListProtectionGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpgrsNextToken' - If you specify a value for @MaxResults@ and you have more protection groups than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects.
--
-- * 'lpgrsResponseStatus' - -- | The response status code.
--
-- * 'lpgrsProtectionGroups' -
listProtectionGroupsResponse ::
  -- | 'lpgrsResponseStatus'
  Int ->
  ListProtectionGroupsResponse
listProtectionGroupsResponse pResponseStatus_ =
  ListProtectionGroupsResponse'
    { _lpgrsNextToken = Nothing,
      _lpgrsResponseStatus = pResponseStatus_,
      _lpgrsProtectionGroups = mempty
    }

-- | If you specify a value for @MaxResults@ and you have more protection groups than the value of MaxResults, AWS Shield Advanced returns this token that you can use in your next request, to get the next batch of objects.
lpgrsNextToken :: Lens' ListProtectionGroupsResponse (Maybe Text)
lpgrsNextToken = lens _lpgrsNextToken (\s a -> s {_lpgrsNextToken = a})

-- | -- | The response status code.
lpgrsResponseStatus :: Lens' ListProtectionGroupsResponse Int
lpgrsResponseStatus = lens _lpgrsResponseStatus (\s a -> s {_lpgrsResponseStatus = a})

-- |
lpgrsProtectionGroups :: Lens' ListProtectionGroupsResponse [ProtectionGroup]
lpgrsProtectionGroups = lens _lpgrsProtectionGroups (\s a -> s {_lpgrsProtectionGroups = a}) . _Coerce

instance NFData ListProtectionGroupsResponse
