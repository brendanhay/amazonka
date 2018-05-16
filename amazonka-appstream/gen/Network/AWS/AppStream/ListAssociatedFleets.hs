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
-- Module      : Network.AWS.AppStream.ListAssociatedFleets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the fleets associated with the specified stack.
--
--
module Network.AWS.AppStream.ListAssociatedFleets
    (
    -- * Creating a Request
      listAssociatedFleets
    , ListAssociatedFleets
    -- * Request Lenses
    , lafNextToken
    , lafStackName

    -- * Destructuring the Response
    , listAssociatedFleetsResponse
    , ListAssociatedFleetsResponse
    -- * Response Lenses
    , lafrsNextToken
    , lafrsNames
    , lafrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAssociatedFleets' smart constructor.
data ListAssociatedFleets = ListAssociatedFleets'
  { _lafNextToken :: !(Maybe Text)
  , _lafStackName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAssociatedFleets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lafNextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- * 'lafStackName' - The name of the stack.
listAssociatedFleets
    :: Text -- ^ 'lafStackName'
    -> ListAssociatedFleets
listAssociatedFleets pStackName_ =
  ListAssociatedFleets' {_lafNextToken = Nothing, _lafStackName = pStackName_}


-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
lafNextToken :: Lens' ListAssociatedFleets (Maybe Text)
lafNextToken = lens _lafNextToken (\ s a -> s{_lafNextToken = a})

-- | The name of the stack.
lafStackName :: Lens' ListAssociatedFleets Text
lafStackName = lens _lafStackName (\ s a -> s{_lafStackName = a})

instance AWSRequest ListAssociatedFleets where
        type Rs ListAssociatedFleets =
             ListAssociatedFleetsResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 ListAssociatedFleetsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Names" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListAssociatedFleets where

instance NFData ListAssociatedFleets where

instance ToHeaders ListAssociatedFleets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.ListAssociatedFleets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAssociatedFleets where
        toJSON ListAssociatedFleets'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lafNextToken,
                  Just ("StackName" .= _lafStackName)])

instance ToPath ListAssociatedFleets where
        toPath = const "/"

instance ToQuery ListAssociatedFleets where
        toQuery = const mempty

-- | /See:/ 'listAssociatedFleetsResponse' smart constructor.
data ListAssociatedFleetsResponse = ListAssociatedFleetsResponse'
  { _lafrsNextToken      :: !(Maybe Text)
  , _lafrsNames          :: !(Maybe [Text])
  , _lafrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAssociatedFleetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lafrsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'lafrsNames' - The names of the fleets.
--
-- * 'lafrsResponseStatus' - -- | The response status code.
listAssociatedFleetsResponse
    :: Int -- ^ 'lafrsResponseStatus'
    -> ListAssociatedFleetsResponse
listAssociatedFleetsResponse pResponseStatus_ =
  ListAssociatedFleetsResponse'
    { _lafrsNextToken = Nothing
    , _lafrsNames = Nothing
    , _lafrsResponseStatus = pResponseStatus_
    }


-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
lafrsNextToken :: Lens' ListAssociatedFleetsResponse (Maybe Text)
lafrsNextToken = lens _lafrsNextToken (\ s a -> s{_lafrsNextToken = a})

-- | The names of the fleets.
lafrsNames :: Lens' ListAssociatedFleetsResponse [Text]
lafrsNames = lens _lafrsNames (\ s a -> s{_lafrsNames = a}) . _Default . _Coerce

-- | -- | The response status code.
lafrsResponseStatus :: Lens' ListAssociatedFleetsResponse Int
lafrsResponseStatus = lens _lafrsResponseStatus (\ s a -> s{_lafrsResponseStatus = a})

instance NFData ListAssociatedFleetsResponse where
