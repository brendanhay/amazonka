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
-- Module      : Network.AWS.IoT.ListAttachedPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the policies attached to the specified thing group.
--
--
module Network.AWS.IoT.ListAttachedPolicies
    (
    -- * Creating a Request
      listAttachedPolicies
    , ListAttachedPolicies
    -- * Request Lenses
    , lapMarker
    , lapRecursive
    , lapPageSize
    , lapTarget

    -- * Destructuring the Response
    , listAttachedPoliciesResponse
    , ListAttachedPoliciesResponse
    -- * Response Lenses
    , laprsNextMarker
    , laprsPolicies
    , laprsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAttachedPolicies' smart constructor.
data ListAttachedPolicies = ListAttachedPolicies'
  { _lapMarker    :: !(Maybe Text)
  , _lapRecursive :: !(Maybe Bool)
  , _lapPageSize  :: !(Maybe Nat)
  , _lapTarget    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttachedPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lapMarker' - The token to retrieve the next set of results.
--
-- * 'lapRecursive' - When true, recursively list attached policies.
--
-- * 'lapPageSize' - The maximum number of results to be returned per request.
--
-- * 'lapTarget' - The group for which the policies will be listed.
listAttachedPolicies
    :: Text -- ^ 'lapTarget'
    -> ListAttachedPolicies
listAttachedPolicies pTarget_ =
  ListAttachedPolicies'
    { _lapMarker = Nothing
    , _lapRecursive = Nothing
    , _lapPageSize = Nothing
    , _lapTarget = pTarget_
    }


-- | The token to retrieve the next set of results.
lapMarker :: Lens' ListAttachedPolicies (Maybe Text)
lapMarker = lens _lapMarker (\ s a -> s{_lapMarker = a})

-- | When true, recursively list attached policies.
lapRecursive :: Lens' ListAttachedPolicies (Maybe Bool)
lapRecursive = lens _lapRecursive (\ s a -> s{_lapRecursive = a})

-- | The maximum number of results to be returned per request.
lapPageSize :: Lens' ListAttachedPolicies (Maybe Natural)
lapPageSize = lens _lapPageSize (\ s a -> s{_lapPageSize = a}) . mapping _Nat

-- | The group for which the policies will be listed.
lapTarget :: Lens' ListAttachedPolicies Text
lapTarget = lens _lapTarget (\ s a -> s{_lapTarget = a})

instance AWSRequest ListAttachedPolicies where
        type Rs ListAttachedPolicies =
             ListAttachedPoliciesResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListAttachedPoliciesResponse' <$>
                   (x .?> "nextMarker") <*>
                     (x .?> "policies" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListAttachedPolicies where

instance NFData ListAttachedPolicies where

instance ToHeaders ListAttachedPolicies where
        toHeaders = const mempty

instance ToJSON ListAttachedPolicies where
        toJSON = const (Object mempty)

instance ToPath ListAttachedPolicies where
        toPath ListAttachedPolicies'{..}
          = mconcat ["/attached-policies/", toBS _lapTarget]

instance ToQuery ListAttachedPolicies where
        toQuery ListAttachedPolicies'{..}
          = mconcat
              ["marker" =: _lapMarker,
               "recursive" =: _lapRecursive,
               "pageSize" =: _lapPageSize]

-- | /See:/ 'listAttachedPoliciesResponse' smart constructor.
data ListAttachedPoliciesResponse = ListAttachedPoliciesResponse'
  { _laprsNextMarker     :: !(Maybe Text)
  , _laprsPolicies       :: !(Maybe [Policy])
  , _laprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttachedPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laprsNextMarker' - The token to retrieve the next set of results, or ``null`` if there are no more results.
--
-- * 'laprsPolicies' - The policies.
--
-- * 'laprsResponseStatus' - -- | The response status code.
listAttachedPoliciesResponse
    :: Int -- ^ 'laprsResponseStatus'
    -> ListAttachedPoliciesResponse
listAttachedPoliciesResponse pResponseStatus_ =
  ListAttachedPoliciesResponse'
    { _laprsNextMarker = Nothing
    , _laprsPolicies = Nothing
    , _laprsResponseStatus = pResponseStatus_
    }


-- | The token to retrieve the next set of results, or ``null`` if there are no more results.
laprsNextMarker :: Lens' ListAttachedPoliciesResponse (Maybe Text)
laprsNextMarker = lens _laprsNextMarker (\ s a -> s{_laprsNextMarker = a})

-- | The policies.
laprsPolicies :: Lens' ListAttachedPoliciesResponse [Policy]
laprsPolicies = lens _laprsPolicies (\ s a -> s{_laprsPolicies = a}) . _Default . _Coerce

-- | -- | The response status code.
laprsResponseStatus :: Lens' ListAttachedPoliciesResponse Int
laprsResponseStatus = lens _laprsResponseStatus (\ s a -> s{_laprsResponseStatus = a})

instance NFData ListAttachedPoliciesResponse where
