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
-- Module      : Network.AWS.CloudHSM.ListLunaClients
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
--
-- Lists all of the clients.
--
-- This operation supports pagination with the use of the @NextToken@ member. If more results are available, the @NextToken@ member of the response contains a token that you pass in the next call to @ListLunaClients@ to retrieve the next set of items.
--
module Network.AWS.CloudHSM.ListLunaClients
    (
    -- * Creating a Request
      listLunaClients
    , ListLunaClients
    -- * Request Lenses
    , llcNextToken

    -- * Destructuring the Response
    , listLunaClientsResponse
    , ListLunaClientsResponse
    -- * Response Lenses
    , llcrsNextToken
    , llcrsResponseStatus
    , llcrsClientList
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listLunaClients' smart constructor.
newtype ListLunaClients = ListLunaClients'
  { _llcNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLunaClients' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llcNextToken' - The @NextToken@ value from a previous call to @ListLunaClients@ . Pass null if this is the first call.
listLunaClients
    :: ListLunaClients
listLunaClients = ListLunaClients' {_llcNextToken = Nothing}


-- | The @NextToken@ value from a previous call to @ListLunaClients@ . Pass null if this is the first call.
llcNextToken :: Lens' ListLunaClients (Maybe Text)
llcNextToken = lens _llcNextToken (\ s a -> s{_llcNextToken = a})

instance AWSRequest ListLunaClients where
        type Rs ListLunaClients = ListLunaClientsResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 ListLunaClientsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "ClientList" .!@ mempty))

instance Hashable ListLunaClients where

instance NFData ListLunaClients where

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
          = object
              (catMaybes [("NextToken" .=) <$> _llcNextToken])

instance ToPath ListLunaClients where
        toPath = const "/"

instance ToQuery ListLunaClients where
        toQuery = const mempty

-- | /See:/ 'listLunaClientsResponse' smart constructor.
data ListLunaClientsResponse = ListLunaClientsResponse'
  { _llcrsNextToken      :: !(Maybe Text)
  , _llcrsResponseStatus :: !Int
  , _llcrsClientList     :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLunaClientsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llcrsNextToken' - If not null, more results are available. Pass this to @ListLunaClients@ to retrieve the next set of items.
--
-- * 'llcrsResponseStatus' - -- | The response status code.
--
-- * 'llcrsClientList' - The list of clients.
listLunaClientsResponse
    :: Int -- ^ 'llcrsResponseStatus'
    -> ListLunaClientsResponse
listLunaClientsResponse pResponseStatus_ =
  ListLunaClientsResponse'
    { _llcrsNextToken = Nothing
    , _llcrsResponseStatus = pResponseStatus_
    , _llcrsClientList = mempty
    }


-- | If not null, more results are available. Pass this to @ListLunaClients@ to retrieve the next set of items.
llcrsNextToken :: Lens' ListLunaClientsResponse (Maybe Text)
llcrsNextToken = lens _llcrsNextToken (\ s a -> s{_llcrsNextToken = a})

-- | -- | The response status code.
llcrsResponseStatus :: Lens' ListLunaClientsResponse Int
llcrsResponseStatus = lens _llcrsResponseStatus (\ s a -> s{_llcrsResponseStatus = a})

-- | The list of clients.
llcrsClientList :: Lens' ListLunaClientsResponse [Text]
llcrsClientList = lens _llcrsClientList (\ s a -> s{_llcrsClientList = a}) . _Coerce

instance NFData ListLunaClientsResponse where
