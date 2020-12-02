{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTriggers
-- Copyright   : (c) 2013-2018 Brendan Hay
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
    (
    -- * Creating a Request
      getTriggers
    , GetTriggers
    -- * Request Lenses
    , gtsNextToken
    , gtsMaxResults
    , gtsDependentJobName

    -- * Destructuring the Response
    , getTriggersResponse
    , GetTriggersResponse
    -- * Response Lenses
    , gttrsTriggers
    , gttrsNextToken
    , gttrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTriggers' smart constructor.
data GetTriggers = GetTriggers'
  { _gtsNextToken        :: !(Maybe Text)
  , _gtsMaxResults       :: !(Maybe Nat)
  , _gtsDependentJobName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTriggers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtsNextToken' - A continuation token, if this is a continuation call.
--
-- * 'gtsMaxResults' - The maximum size of the response.
--
-- * 'gtsDependentJobName' - The name of the job for which to retrieve triggers. The trigger that can start this job will be returned, and if there is no such trigger, all triggers will be returned.
getTriggers
    :: GetTriggers
getTriggers =
  GetTriggers'
    { _gtsNextToken = Nothing
    , _gtsMaxResults = Nothing
    , _gtsDependentJobName = Nothing
    }


-- | A continuation token, if this is a continuation call.
gtsNextToken :: Lens' GetTriggers (Maybe Text)
gtsNextToken = lens _gtsNextToken (\ s a -> s{_gtsNextToken = a})

-- | The maximum size of the response.
gtsMaxResults :: Lens' GetTriggers (Maybe Natural)
gtsMaxResults = lens _gtsMaxResults (\ s a -> s{_gtsMaxResults = a}) . mapping _Nat

-- | The name of the job for which to retrieve triggers. The trigger that can start this job will be returned, and if there is no such trigger, all triggers will be returned.
gtsDependentJobName :: Lens' GetTriggers (Maybe Text)
gtsDependentJobName = lens _gtsDependentJobName (\ s a -> s{_gtsDependentJobName = a})

instance AWSPager GetTriggers where
        page rq rs
          | stop (rs ^. gttrsNextToken) = Nothing
          | stop (rs ^. gttrsTriggers) = Nothing
          | otherwise =
            Just $ rq & gtsNextToken .~ rs ^. gttrsNextToken

instance AWSRequest GetTriggers where
        type Rs GetTriggers = GetTriggersResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetTriggersResponse' <$>
                   (x .?> "Triggers" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetTriggers where

instance NFData GetTriggers where

instance ToHeaders GetTriggers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetTriggers" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTriggers where
        toJSON GetTriggers'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gtsNextToken,
                  ("MaxResults" .=) <$> _gtsMaxResults,
                  ("DependentJobName" .=) <$> _gtsDependentJobName])

instance ToPath GetTriggers where
        toPath = const "/"

instance ToQuery GetTriggers where
        toQuery = const mempty

-- | /See:/ 'getTriggersResponse' smart constructor.
data GetTriggersResponse = GetTriggersResponse'
  { _gttrsTriggers       :: !(Maybe [Trigger])
  , _gttrsNextToken      :: !(Maybe Text)
  , _gttrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTriggersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gttrsTriggers' - A list of triggers for the specified job.
--
-- * 'gttrsNextToken' - A continuation token, if not all the requested triggers have yet been returned.
--
-- * 'gttrsResponseStatus' - -- | The response status code.
getTriggersResponse
    :: Int -- ^ 'gttrsResponseStatus'
    -> GetTriggersResponse
getTriggersResponse pResponseStatus_ =
  GetTriggersResponse'
    { _gttrsTriggers = Nothing
    , _gttrsNextToken = Nothing
    , _gttrsResponseStatus = pResponseStatus_
    }


-- | A list of triggers for the specified job.
gttrsTriggers :: Lens' GetTriggersResponse [Trigger]
gttrsTriggers = lens _gttrsTriggers (\ s a -> s{_gttrsTriggers = a}) . _Default . _Coerce

-- | A continuation token, if not all the requested triggers have yet been returned.
gttrsNextToken :: Lens' GetTriggersResponse (Maybe Text)
gttrsNextToken = lens _gttrsNextToken (\ s a -> s{_gttrsNextToken = a})

-- | -- | The response status code.
gttrsResponseStatus :: Lens' GetTriggersResponse Int
gttrsResponseStatus = lens _gttrsResponseStatus (\ s a -> s{_gttrsResponseStatus = a})

instance NFData GetTriggersResponse where
