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
-- Module      : Network.AWS.CloudWatchEvents.ListPartnerEventSourceAccounts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to display the AWS account ID that a particular partner event source name is associated with.
--
--
module Network.AWS.CloudWatchEvents.ListPartnerEventSourceAccounts
    (
    -- * Creating a Request
      listPartnerEventSourceAccounts
    , ListPartnerEventSourceAccounts
    -- * Request Lenses
    , lpesaNextToken
    , lpesaLimit
    , lpesaEventSourceName

    -- * Destructuring the Response
    , listPartnerEventSourceAccountsResponse
    , ListPartnerEventSourceAccountsResponse
    -- * Response Lenses
    , lpesarsPartnerEventSourceAccounts
    , lpesarsNextToken
    , lpesarsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPartnerEventSourceAccounts' smart constructor.
data ListPartnerEventSourceAccounts = ListPartnerEventSourceAccounts'
  { _lpesaNextToken       :: !(Maybe Text)
  , _lpesaLimit           :: !(Maybe Nat)
  , _lpesaEventSourceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPartnerEventSourceAccounts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpesaNextToken' - The token returned by a previous call to this operation. Specifying this retrieves the next set of results.
--
-- * 'lpesaLimit' - Specifying this limits the number of results returned by this operation. The operation also returns a @NextToken@ that you can use in a subsequent operation to retrieve the next set of results.
--
-- * 'lpesaEventSourceName' - The name of the partner event source to display account information about.
listPartnerEventSourceAccounts
    :: Text -- ^ 'lpesaEventSourceName'
    -> ListPartnerEventSourceAccounts
listPartnerEventSourceAccounts pEventSourceName_ =
  ListPartnerEventSourceAccounts'
    { _lpesaNextToken = Nothing
    , _lpesaLimit = Nothing
    , _lpesaEventSourceName = pEventSourceName_
    }


-- | The token returned by a previous call to this operation. Specifying this retrieves the next set of results.
lpesaNextToken :: Lens' ListPartnerEventSourceAccounts (Maybe Text)
lpesaNextToken = lens _lpesaNextToken (\ s a -> s{_lpesaNextToken = a})

-- | Specifying this limits the number of results returned by this operation. The operation also returns a @NextToken@ that you can use in a subsequent operation to retrieve the next set of results.
lpesaLimit :: Lens' ListPartnerEventSourceAccounts (Maybe Natural)
lpesaLimit = lens _lpesaLimit (\ s a -> s{_lpesaLimit = a}) . mapping _Nat

-- | The name of the partner event source to display account information about.
lpesaEventSourceName :: Lens' ListPartnerEventSourceAccounts Text
lpesaEventSourceName = lens _lpesaEventSourceName (\ s a -> s{_lpesaEventSourceName = a})

instance AWSRequest ListPartnerEventSourceAccounts
         where
        type Rs ListPartnerEventSourceAccounts =
             ListPartnerEventSourceAccountsResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 ListPartnerEventSourceAccountsResponse' <$>
                   (x .?> "PartnerEventSourceAccounts" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListPartnerEventSourceAccounts
         where

instance NFData ListPartnerEventSourceAccounts where

instance ToHeaders ListPartnerEventSourceAccounts
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.ListPartnerEventSourceAccounts" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPartnerEventSourceAccounts where
        toJSON ListPartnerEventSourceAccounts'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lpesaNextToken,
                  ("Limit" .=) <$> _lpesaLimit,
                  Just ("EventSourceName" .= _lpesaEventSourceName)])

instance ToPath ListPartnerEventSourceAccounts where
        toPath = const "/"

instance ToQuery ListPartnerEventSourceAccounts where
        toQuery = const mempty

-- | /See:/ 'listPartnerEventSourceAccountsResponse' smart constructor.
data ListPartnerEventSourceAccountsResponse = ListPartnerEventSourceAccountsResponse'
  { _lpesarsPartnerEventSourceAccounts :: !(Maybe [PartnerEventSourceAccount])
  , _lpesarsNextToken                  :: !(Maybe Text)
  , _lpesarsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPartnerEventSourceAccountsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpesarsPartnerEventSourceAccounts' - The list of partner event sources returned by the operation.
--
-- * 'lpesarsNextToken' - A token you can use in a subsequent operation to retrieve the next set of results.
--
-- * 'lpesarsResponseStatus' - -- | The response status code.
listPartnerEventSourceAccountsResponse
    :: Int -- ^ 'lpesarsResponseStatus'
    -> ListPartnerEventSourceAccountsResponse
listPartnerEventSourceAccountsResponse pResponseStatus_ =
  ListPartnerEventSourceAccountsResponse'
    { _lpesarsPartnerEventSourceAccounts = Nothing
    , _lpesarsNextToken = Nothing
    , _lpesarsResponseStatus = pResponseStatus_
    }


-- | The list of partner event sources returned by the operation.
lpesarsPartnerEventSourceAccounts :: Lens' ListPartnerEventSourceAccountsResponse [PartnerEventSourceAccount]
lpesarsPartnerEventSourceAccounts = lens _lpesarsPartnerEventSourceAccounts (\ s a -> s{_lpesarsPartnerEventSourceAccounts = a}) . _Default . _Coerce

-- | A token you can use in a subsequent operation to retrieve the next set of results.
lpesarsNextToken :: Lens' ListPartnerEventSourceAccountsResponse (Maybe Text)
lpesarsNextToken = lens _lpesarsNextToken (\ s a -> s{_lpesarsNextToken = a})

-- | -- | The response status code.
lpesarsResponseStatus :: Lens' ListPartnerEventSourceAccountsResponse Int
lpesarsResponseStatus = lens _lpesarsResponseStatus (\ s a -> s{_lpesarsResponseStatus = a})

instance NFData
           ListPartnerEventSourceAccountsResponse
         where
