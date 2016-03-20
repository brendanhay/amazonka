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
-- Module      : Network.AWS.IoT.ListPrincipalThings
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things associated with the specified principal.
module Network.AWS.IoT.ListPrincipalThings
    (
    -- * Creating a Request
      listPrincipalThings
    , ListPrincipalThings
    -- * Request Lenses
    , lptNextToken
    , lptMaxResults
    , lptPrincipal

    -- * Destructuring the Response
    , listPrincipalThingsResponse
    , ListPrincipalThingsResponse
    -- * Response Lenses
    , lptrsNextToken
    , lptrsThings
    , lptrsResponseStatus
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the ListPrincipalThings operation.
--
-- /See:/ 'listPrincipalThings' smart constructor.
data ListPrincipalThings = ListPrincipalThings'
    { _lptNextToken  :: !(Maybe Text)
    , _lptMaxResults :: !(Maybe Nat)
    , _lptPrincipal  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListPrincipalThings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lptNextToken'
--
-- * 'lptMaxResults'
--
-- * 'lptPrincipal'
listPrincipalThings
    :: Text -- ^ 'lptPrincipal'
    -> ListPrincipalThings
listPrincipalThings pPrincipal_ =
    ListPrincipalThings'
    { _lptNextToken = Nothing
    , _lptMaxResults = Nothing
    , _lptPrincipal = pPrincipal_
    }

-- | A token used to retrieve the next value.
lptNextToken :: Lens' ListPrincipalThings (Maybe Text)
lptNextToken = lens _lptNextToken (\ s a -> s{_lptNextToken = a});

-- | The maximum number of principals to return.
lptMaxResults :: Lens' ListPrincipalThings (Maybe Natural)
lptMaxResults = lens _lptMaxResults (\ s a -> s{_lptMaxResults = a}) . mapping _Nat;

-- | The principal.
lptPrincipal :: Lens' ListPrincipalThings Text
lptPrincipal = lens _lptPrincipal (\ s a -> s{_lptPrincipal = a});

instance AWSRequest ListPrincipalThings where
        type Rs ListPrincipalThings =
             ListPrincipalThingsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListPrincipalThingsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "things" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPrincipalThings

instance ToHeaders ListPrincipalThings where
        toHeaders ListPrincipalThings'{..}
          = mconcat ["x-amzn-principal" =# _lptPrincipal]

instance ToPath ListPrincipalThings where
        toPath = const "/principals/things"

instance ToQuery ListPrincipalThings where
        toQuery ListPrincipalThings'{..}
          = mconcat
              ["nextToken" =: _lptNextToken,
               "maxResults" =: _lptMaxResults]

-- | The output from the ListPrincipalThings operation.
--
-- /See:/ 'listPrincipalThingsResponse' smart constructor.
data ListPrincipalThingsResponse = ListPrincipalThingsResponse'
    { _lptrsNextToken      :: !(Maybe Text)
    , _lptrsThings         :: !(Maybe [Text])
    , _lptrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListPrincipalThingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lptrsNextToken'
--
-- * 'lptrsThings'
--
-- * 'lptrsResponseStatus'
listPrincipalThingsResponse
    :: Int -- ^ 'lptrsResponseStatus'
    -> ListPrincipalThingsResponse
listPrincipalThingsResponse pResponseStatus_ =
    ListPrincipalThingsResponse'
    { _lptrsNextToken = Nothing
    , _lptrsThings = Nothing
    , _lptrsResponseStatus = pResponseStatus_
    }

-- | A token used to retrieve the next value.
lptrsNextToken :: Lens' ListPrincipalThingsResponse (Maybe Text)
lptrsNextToken = lens _lptrsNextToken (\ s a -> s{_lptrsNextToken = a});

-- | The things.
lptrsThings :: Lens' ListPrincipalThingsResponse [Text]
lptrsThings = lens _lptrsThings (\ s a -> s{_lptrsThings = a}) . _Default . _Coerce;

-- | The response status code.
lptrsResponseStatus :: Lens' ListPrincipalThingsResponse Int
lptrsResponseStatus = lens _lptrsResponseStatus (\ s a -> s{_lptrsResponseStatus = a});
