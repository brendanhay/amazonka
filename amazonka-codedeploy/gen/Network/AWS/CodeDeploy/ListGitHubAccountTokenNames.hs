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
-- Module      : Network.AWS.CodeDeploy.ListGitHubAccountTokenNames
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of stored connections to GitHub accounts.
--
--
module Network.AWS.CodeDeploy.ListGitHubAccountTokenNames
    (
    -- * Creating a Request
      listGitHubAccountTokenNames
    , ListGitHubAccountTokenNames
    -- * Request Lenses
    , lghatnNextToken

    -- * Destructuring the Response
    , listGitHubAccountTokenNamesResponse
    , ListGitHubAccountTokenNamesResponse
    -- * Response Lenses
    , lghatnrsTokenNameList
    , lghatnrsNextToken
    , lghatnrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a ListGitHubAccountTokenNames operation.
--
--
--
-- /See:/ 'listGitHubAccountTokenNames' smart constructor.
newtype ListGitHubAccountTokenNames = ListGitHubAccountTokenNames'
  { _lghatnNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGitHubAccountTokenNames' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lghatnNextToken' - An identifier returned from the previous ListGitHubAccountTokenNames call. It can be used to return the next set of names in the list.
listGitHubAccountTokenNames
    :: ListGitHubAccountTokenNames
listGitHubAccountTokenNames =
  ListGitHubAccountTokenNames' {_lghatnNextToken = Nothing}


-- | An identifier returned from the previous ListGitHubAccountTokenNames call. It can be used to return the next set of names in the list.
lghatnNextToken :: Lens' ListGitHubAccountTokenNames (Maybe Text)
lghatnNextToken = lens _lghatnNextToken (\ s a -> s{_lghatnNextToken = a})

instance AWSRequest ListGitHubAccountTokenNames where
        type Rs ListGitHubAccountTokenNames =
             ListGitHubAccountTokenNamesResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 ListGitHubAccountTokenNamesResponse' <$>
                   (x .?> "tokenNameList" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListGitHubAccountTokenNames where

instance NFData ListGitHubAccountTokenNames where

instance ToHeaders ListGitHubAccountTokenNames where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.ListGitHubAccountTokenNames" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListGitHubAccountTokenNames where
        toJSON ListGitHubAccountTokenNames'{..}
          = object
              (catMaybes [("nextToken" .=) <$> _lghatnNextToken])

instance ToPath ListGitHubAccountTokenNames where
        toPath = const "/"

instance ToQuery ListGitHubAccountTokenNames where
        toQuery = const mempty

-- | Represents the output of a ListGitHubAccountTokenNames operation.
--
--
--
-- /See:/ 'listGitHubAccountTokenNamesResponse' smart constructor.
data ListGitHubAccountTokenNamesResponse = ListGitHubAccountTokenNamesResponse'
  { _lghatnrsTokenNameList  :: !(Maybe [Text])
  , _lghatnrsNextToken      :: !(Maybe Text)
  , _lghatnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGitHubAccountTokenNamesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lghatnrsTokenNameList' - A list of names of connections to GitHub accounts.
--
-- * 'lghatnrsNextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent ListGitHubAccountTokenNames call to return the next set of names in the list.
--
-- * 'lghatnrsResponseStatus' - -- | The response status code.
listGitHubAccountTokenNamesResponse
    :: Int -- ^ 'lghatnrsResponseStatus'
    -> ListGitHubAccountTokenNamesResponse
listGitHubAccountTokenNamesResponse pResponseStatus_ =
  ListGitHubAccountTokenNamesResponse'
    { _lghatnrsTokenNameList = Nothing
    , _lghatnrsNextToken = Nothing
    , _lghatnrsResponseStatus = pResponseStatus_
    }


-- | A list of names of connections to GitHub accounts.
lghatnrsTokenNameList :: Lens' ListGitHubAccountTokenNamesResponse [Text]
lghatnrsTokenNameList = lens _lghatnrsTokenNameList (\ s a -> s{_lghatnrsTokenNameList = a}) . _Default . _Coerce

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent ListGitHubAccountTokenNames call to return the next set of names in the list.
lghatnrsNextToken :: Lens' ListGitHubAccountTokenNamesResponse (Maybe Text)
lghatnrsNextToken = lens _lghatnrsNextToken (\ s a -> s{_lghatnrsNextToken = a})

-- | -- | The response status code.
lghatnrsResponseStatus :: Lens' ListGitHubAccountTokenNamesResponse Int
lghatnrsResponseStatus = lens _lghatnrsResponseStatus (\ s a -> s{_lghatnrsResponseStatus = a})

instance NFData ListGitHubAccountTokenNamesResponse
         where
