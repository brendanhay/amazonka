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
-- Module      : Network.AWS.Glue.GetTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the triggers associated with a job.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetTriggers
  ( -- * Creating a Request
    getTriggers,
    GetTriggers,

    -- * Request Lenses
    gtNextToken,
    gtMaxResults,
    gtDependentJobName,

    -- * Destructuring the Response
    getTriggersResponse,
    GetTriggersResponse,

    -- * Response Lenses
    gttrsTriggers,
    gttrsNextToken,
    gttrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTriggers' smart constructor.
data GetTriggers = GetTriggers'
  { _gtNextToken :: !(Maybe Text),
    _gtMaxResults :: !(Maybe Nat),
    _gtDependentJobName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTriggers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtNextToken' - A continuation token, if this is a continuation call.
--
-- * 'gtMaxResults' - The maximum size of the response.
--
-- * 'gtDependentJobName' - The name of the job to retrieve triggers for. The trigger that can start this job is returned, and if there is no such trigger, all triggers are returned.
getTriggers ::
  GetTriggers
getTriggers =
  GetTriggers'
    { _gtNextToken = Nothing,
      _gtMaxResults = Nothing,
      _gtDependentJobName = Nothing
    }

-- | A continuation token, if this is a continuation call.
gtNextToken :: Lens' GetTriggers (Maybe Text)
gtNextToken = lens _gtNextToken (\s a -> s {_gtNextToken = a})

-- | The maximum size of the response.
gtMaxResults :: Lens' GetTriggers (Maybe Natural)
gtMaxResults = lens _gtMaxResults (\s a -> s {_gtMaxResults = a}) . mapping _Nat

-- | The name of the job to retrieve triggers for. The trigger that can start this job is returned, and if there is no such trigger, all triggers are returned.
gtDependentJobName :: Lens' GetTriggers (Maybe Text)
gtDependentJobName = lens _gtDependentJobName (\s a -> s {_gtDependentJobName = a})

instance AWSPager GetTriggers where
  page rq rs
    | stop (rs ^. gttrsNextToken) = Nothing
    | stop (rs ^. gttrsTriggers) = Nothing
    | otherwise = Just $ rq & gtNextToken .~ rs ^. gttrsNextToken

instance AWSRequest GetTriggers where
  type Rs GetTriggers = GetTriggersResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetTriggersResponse'
            <$> (x .?> "Triggers" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetTriggers

instance NFData GetTriggers

instance ToHeaders GetTriggers where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetTriggers" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetTriggers where
  toJSON GetTriggers' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _gtNextToken,
            ("MaxResults" .=) <$> _gtMaxResults,
            ("DependentJobName" .=) <$> _gtDependentJobName
          ]
      )

instance ToPath GetTriggers where
  toPath = const "/"

instance ToQuery GetTriggers where
  toQuery = const mempty

-- | /See:/ 'getTriggersResponse' smart constructor.
data GetTriggersResponse = GetTriggersResponse'
  { _gttrsTriggers ::
      !(Maybe [Trigger]),
    _gttrsNextToken :: !(Maybe Text),
    _gttrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTriggersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gttrsTriggers' - A list of triggers for the specified job.
--
-- * 'gttrsNextToken' - A continuation token, if not all the requested triggers have yet been returned.
--
-- * 'gttrsResponseStatus' - -- | The response status code.
getTriggersResponse ::
  -- | 'gttrsResponseStatus'
  Int ->
  GetTriggersResponse
getTriggersResponse pResponseStatus_ =
  GetTriggersResponse'
    { _gttrsTriggers = Nothing,
      _gttrsNextToken = Nothing,
      _gttrsResponseStatus = pResponseStatus_
    }

-- | A list of triggers for the specified job.
gttrsTriggers :: Lens' GetTriggersResponse [Trigger]
gttrsTriggers = lens _gttrsTriggers (\s a -> s {_gttrsTriggers = a}) . _Default . _Coerce

-- | A continuation token, if not all the requested triggers have yet been returned.
gttrsNextToken :: Lens' GetTriggersResponse (Maybe Text)
gttrsNextToken = lens _gttrsNextToken (\s a -> s {_gttrsNextToken = a})

-- | -- | The response status code.
gttrsResponseStatus :: Lens' GetTriggersResponse Int
gttrsResponseStatus = lens _gttrsResponseStatus (\s a -> s {_gttrsResponseStatus = a})

instance NFData GetTriggersResponse
