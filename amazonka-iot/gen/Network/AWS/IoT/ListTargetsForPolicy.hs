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
-- Module      : Network.AWS.IoT.ListTargetsForPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List targets for the specified policy.
--
--
module Network.AWS.IoT.ListTargetsForPolicy
    (
    -- * Creating a Request
      listTargetsForPolicy
    , ListTargetsForPolicy
    -- * Request Lenses
    , ltfpMarker
    , ltfpPageSize
    , ltfpPolicyName

    -- * Destructuring the Response
    , listTargetsForPolicyResponse
    , ListTargetsForPolicyResponse
    -- * Response Lenses
    , ltfprsTargets
    , ltfprsNextMarker
    , ltfprsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTargetsForPolicy' smart constructor.
data ListTargetsForPolicy = ListTargetsForPolicy'
  { _ltfpMarker     :: !(Maybe Text)
  , _ltfpPageSize   :: !(Maybe Nat)
  , _ltfpPolicyName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTargetsForPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfpMarker' - A marker used to get the next set of results.
--
-- * 'ltfpPageSize' - The maximum number of results to return at one time.
--
-- * 'ltfpPolicyName' - The policy name.
listTargetsForPolicy
    :: Text -- ^ 'ltfpPolicyName'
    -> ListTargetsForPolicy
listTargetsForPolicy pPolicyName_ =
  ListTargetsForPolicy'
    { _ltfpMarker = Nothing
    , _ltfpPageSize = Nothing
    , _ltfpPolicyName = pPolicyName_
    }


-- | A marker used to get the next set of results.
ltfpMarker :: Lens' ListTargetsForPolicy (Maybe Text)
ltfpMarker = lens _ltfpMarker (\ s a -> s{_ltfpMarker = a})

-- | The maximum number of results to return at one time.
ltfpPageSize :: Lens' ListTargetsForPolicy (Maybe Natural)
ltfpPageSize = lens _ltfpPageSize (\ s a -> s{_ltfpPageSize = a}) . mapping _Nat

-- | The policy name.
ltfpPolicyName :: Lens' ListTargetsForPolicy Text
ltfpPolicyName = lens _ltfpPolicyName (\ s a -> s{_ltfpPolicyName = a})

instance AWSRequest ListTargetsForPolicy where
        type Rs ListTargetsForPolicy =
             ListTargetsForPolicyResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListTargetsForPolicyResponse' <$>
                   (x .?> "targets" .!@ mempty) <*> (x .?> "nextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListTargetsForPolicy where

instance NFData ListTargetsForPolicy where

instance ToHeaders ListTargetsForPolicy where
        toHeaders = const mempty

instance ToJSON ListTargetsForPolicy where
        toJSON = const (Object mempty)

instance ToPath ListTargetsForPolicy where
        toPath ListTargetsForPolicy'{..}
          = mconcat ["/policy-targets/", toBS _ltfpPolicyName]

instance ToQuery ListTargetsForPolicy where
        toQuery ListTargetsForPolicy'{..}
          = mconcat
              ["marker" =: _ltfpMarker,
               "pageSize" =: _ltfpPageSize]

-- | /See:/ 'listTargetsForPolicyResponse' smart constructor.
data ListTargetsForPolicyResponse = ListTargetsForPolicyResponse'
  { _ltfprsTargets        :: !(Maybe [Text])
  , _ltfprsNextMarker     :: !(Maybe Text)
  , _ltfprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTargetsForPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfprsTargets' - The policy targets.
--
-- * 'ltfprsNextMarker' - A marker used to get the next set of results.
--
-- * 'ltfprsResponseStatus' - -- | The response status code.
listTargetsForPolicyResponse
    :: Int -- ^ 'ltfprsResponseStatus'
    -> ListTargetsForPolicyResponse
listTargetsForPolicyResponse pResponseStatus_ =
  ListTargetsForPolicyResponse'
    { _ltfprsTargets = Nothing
    , _ltfprsNextMarker = Nothing
    , _ltfprsResponseStatus = pResponseStatus_
    }


-- | The policy targets.
ltfprsTargets :: Lens' ListTargetsForPolicyResponse [Text]
ltfprsTargets = lens _ltfprsTargets (\ s a -> s{_ltfprsTargets = a}) . _Default . _Coerce

-- | A marker used to get the next set of results.
ltfprsNextMarker :: Lens' ListTargetsForPolicyResponse (Maybe Text)
ltfprsNextMarker = lens _ltfprsNextMarker (\ s a -> s{_ltfprsNextMarker = a})

-- | -- | The response status code.
ltfprsResponseStatus :: Lens' ListTargetsForPolicyResponse Int
ltfprsResponseStatus = lens _ltfprsResponseStatus (\ s a -> s{_ltfprsResponseStatus = a})

instance NFData ListTargetsForPolicyResponse where
