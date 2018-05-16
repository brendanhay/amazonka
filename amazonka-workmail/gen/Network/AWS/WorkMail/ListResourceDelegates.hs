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
-- Module      : Network.AWS.WorkMail.ListResourceDelegates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the delegates associated with a resource. Users and groups can be resource delegates and answer requests on behalf of the resource.
--
--
module Network.AWS.WorkMail.ListResourceDelegates
    (
    -- * Creating a Request
      listResourceDelegates
    , ListResourceDelegates
    -- * Request Lenses
    , lrdNextToken
    , lrdMaxResults
    , lrdOrganizationId
    , lrdResourceId

    -- * Destructuring the Response
    , listResourceDelegatesResponse
    , ListResourceDelegatesResponse
    -- * Response Lenses
    , lrdrsDelegates
    , lrdrsNextToken
    , lrdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'listResourceDelegates' smart constructor.
data ListResourceDelegates = ListResourceDelegates'
  { _lrdNextToken      :: !(Maybe Text)
  , _lrdMaxResults     :: !(Maybe Nat)
  , _lrdOrganizationId :: !Text
  , _lrdResourceId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceDelegates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdNextToken' - The token used to paginate through the delegates associated with a resource.
--
-- * 'lrdMaxResults' - The number of maximum results in a page.
--
-- * 'lrdOrganizationId' - The identifier for the organization that contains the resource for which delegates are listed.
--
-- * 'lrdResourceId' - The identifier for the resource whose delegates are listed.
listResourceDelegates
    :: Text -- ^ 'lrdOrganizationId'
    -> Text -- ^ 'lrdResourceId'
    -> ListResourceDelegates
listResourceDelegates pOrganizationId_ pResourceId_ =
  ListResourceDelegates'
    { _lrdNextToken = Nothing
    , _lrdMaxResults = Nothing
    , _lrdOrganizationId = pOrganizationId_
    , _lrdResourceId = pResourceId_
    }


-- | The token used to paginate through the delegates associated with a resource.
lrdNextToken :: Lens' ListResourceDelegates (Maybe Text)
lrdNextToken = lens _lrdNextToken (\ s a -> s{_lrdNextToken = a})

-- | The number of maximum results in a page.
lrdMaxResults :: Lens' ListResourceDelegates (Maybe Natural)
lrdMaxResults = lens _lrdMaxResults (\ s a -> s{_lrdMaxResults = a}) . mapping _Nat

-- | The identifier for the organization that contains the resource for which delegates are listed.
lrdOrganizationId :: Lens' ListResourceDelegates Text
lrdOrganizationId = lens _lrdOrganizationId (\ s a -> s{_lrdOrganizationId = a})

-- | The identifier for the resource whose delegates are listed.
lrdResourceId :: Lens' ListResourceDelegates Text
lrdResourceId = lens _lrdResourceId (\ s a -> s{_lrdResourceId = a})

instance AWSRequest ListResourceDelegates where
        type Rs ListResourceDelegates =
             ListResourceDelegatesResponse
        request = postJSON workMail
        response
          = receiveJSON
              (\ s h x ->
                 ListResourceDelegatesResponse' <$>
                   (x .?> "Delegates" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListResourceDelegates where

instance NFData ListResourceDelegates where

instance ToHeaders ListResourceDelegates where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.ListResourceDelegates" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListResourceDelegates where
        toJSON ListResourceDelegates'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lrdNextToken,
                  ("MaxResults" .=) <$> _lrdMaxResults,
                  Just ("OrganizationId" .= _lrdOrganizationId),
                  Just ("ResourceId" .= _lrdResourceId)])

instance ToPath ListResourceDelegates where
        toPath = const "/"

instance ToQuery ListResourceDelegates where
        toQuery = const mempty

-- | /See:/ 'listResourceDelegatesResponse' smart constructor.
data ListResourceDelegatesResponse = ListResourceDelegatesResponse'
  { _lrdrsDelegates      :: !(Maybe [Delegate])
  , _lrdrsNextToken      :: !(Maybe Text)
  , _lrdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceDelegatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdrsDelegates' - One page of the resource's delegates.
--
-- * 'lrdrsNextToken' - The token used to paginate through the delegates associated with a resource. While results are still available, it has an associated value. When the last page is reached, the token is empty.
--
-- * 'lrdrsResponseStatus' - -- | The response status code.
listResourceDelegatesResponse
    :: Int -- ^ 'lrdrsResponseStatus'
    -> ListResourceDelegatesResponse
listResourceDelegatesResponse pResponseStatus_ =
  ListResourceDelegatesResponse'
    { _lrdrsDelegates = Nothing
    , _lrdrsNextToken = Nothing
    , _lrdrsResponseStatus = pResponseStatus_
    }


-- | One page of the resource's delegates.
lrdrsDelegates :: Lens' ListResourceDelegatesResponse [Delegate]
lrdrsDelegates = lens _lrdrsDelegates (\ s a -> s{_lrdrsDelegates = a}) . _Default . _Coerce

-- | The token used to paginate through the delegates associated with a resource. While results are still available, it has an associated value. When the last page is reached, the token is empty.
lrdrsNextToken :: Lens' ListResourceDelegatesResponse (Maybe Text)
lrdrsNextToken = lens _lrdrsNextToken (\ s a -> s{_lrdrsNextToken = a})

-- | -- | The response status code.
lrdrsResponseStatus :: Lens' ListResourceDelegatesResponse Int
lrdrsResponseStatus = lens _lrdrsResponseStatus (\ s a -> s{_lrdrsResponseStatus = a})

instance NFData ListResourceDelegatesResponse where
