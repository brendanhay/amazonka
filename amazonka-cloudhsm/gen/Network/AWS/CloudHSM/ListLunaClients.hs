{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ListLunaClients
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the clients.
--
-- This operation supports pagination with the use of the /NextToken/
-- member. If more results are available, the /NextToken/ member of the
-- response contains a token that you pass in the next call to
-- ListLunaClients to retrieve the next set of items.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ListLunaClients.html>
module Network.AWS.CloudHSM.ListLunaClients
    (
    -- * Request
      ListLunaClients
    -- ** Request constructor
    , listLunaClients
    -- ** Request lenses
    , llcrqNextToken

    -- * Response
    , ListLunaClientsResponse
    -- ** Response constructor
    , listLunaClientsResponse
    -- ** Response lenses
    , llcrsNextToken
    , llcrsStatus
    , llcrsClientList
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listLunaClients' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'llcrqNextToken'
newtype ListLunaClients = ListLunaClients'
    { _llcrqNextToken :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListLunaClients' smart constructor.
listLunaClients :: ListLunaClients
listLunaClients =
    ListLunaClients'
    { _llcrqNextToken = Nothing
    }

-- | The /NextToken/ value from a previous call to ListLunaClients. Pass null
-- if this is the first call.
llcrqNextToken :: Lens' ListLunaClients (Maybe Text)
llcrqNextToken = lens _llcrqNextToken (\ s a -> s{_llcrqNextToken = a});

instance AWSRequest ListLunaClients where
        type Sv ListLunaClients = CloudHSM
        type Rs ListLunaClients = ListLunaClientsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListLunaClientsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "ClientList" .!@ mempty))

instance ToHeaders ListLunaClients where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ListLunaClients" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListLunaClients where
        toJSON ListLunaClients'{..}
          = object ["NextToken" .= _llcrqNextToken]

instance ToPath ListLunaClients where
        toPath = const "/"

instance ToQuery ListLunaClients where
        toQuery = const mempty

-- | /See:/ 'listLunaClientsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'llcrsNextToken'
--
-- * 'llcrsStatus'
--
-- * 'llcrsClientList'
data ListLunaClientsResponse = ListLunaClientsResponse'
    { _llcrsNextToken  :: !(Maybe Text)
    , _llcrsStatus     :: !Int
    , _llcrsClientList :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListLunaClientsResponse' smart constructor.
listLunaClientsResponse :: Int -> ListLunaClientsResponse
listLunaClientsResponse pStatus =
    ListLunaClientsResponse'
    { _llcrsNextToken = Nothing
    , _llcrsStatus = pStatus
    , _llcrsClientList = mempty
    }

-- | If not null, more results are available. Pass this to ListLunaClients to
-- retrieve the next set of items.
llcrsNextToken :: Lens' ListLunaClientsResponse (Maybe Text)
llcrsNextToken = lens _llcrsNextToken (\ s a -> s{_llcrsNextToken = a});

-- | FIXME: Undocumented member.
llcrsStatus :: Lens' ListLunaClientsResponse Int
llcrsStatus = lens _llcrsStatus (\ s a -> s{_llcrsStatus = a});

-- | The list of clients.
llcrsClientList :: Lens' ListLunaClientsResponse [Text]
llcrsClientList = lens _llcrsClientList (\ s a -> s{_llcrsClientList = a});
