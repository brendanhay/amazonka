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
-- Module      : Network.AWS.AppStream.ListAssociatedStacks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the stacks associated with the specified fleet.
--
--
module Network.AWS.AppStream.ListAssociatedStacks
    (
    -- * Creating a Request
      listAssociatedStacks
    , ListAssociatedStacks
    -- * Request Lenses
    , lasNextToken
    , lasFleetName

    -- * Destructuring the Response
    , listAssociatedStacksResponse
    , ListAssociatedStacksResponse
    -- * Response Lenses
    , lasrsNextToken
    , lasrsNames
    , lasrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAssociatedStacks' smart constructor.
data ListAssociatedStacks = ListAssociatedStacks'
  { _lasNextToken :: !(Maybe Text)
  , _lasFleetName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAssociatedStacks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lasNextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- * 'lasFleetName' - The name of the fleet.
listAssociatedStacks
    :: Text -- ^ 'lasFleetName'
    -> ListAssociatedStacks
listAssociatedStacks pFleetName_ =
  ListAssociatedStacks' {_lasNextToken = Nothing, _lasFleetName = pFleetName_}


-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
lasNextToken :: Lens' ListAssociatedStacks (Maybe Text)
lasNextToken = lens _lasNextToken (\ s a -> s{_lasNextToken = a})

-- | The name of the fleet.
lasFleetName :: Lens' ListAssociatedStacks Text
lasFleetName = lens _lasFleetName (\ s a -> s{_lasFleetName = a})

instance AWSRequest ListAssociatedStacks where
        type Rs ListAssociatedStacks =
             ListAssociatedStacksResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 ListAssociatedStacksResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Names" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListAssociatedStacks where

instance NFData ListAssociatedStacks where

instance ToHeaders ListAssociatedStacks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.ListAssociatedStacks" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAssociatedStacks where
        toJSON ListAssociatedStacks'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lasNextToken,
                  Just ("FleetName" .= _lasFleetName)])

instance ToPath ListAssociatedStacks where
        toPath = const "/"

instance ToQuery ListAssociatedStacks where
        toQuery = const mempty

-- | /See:/ 'listAssociatedStacksResponse' smart constructor.
data ListAssociatedStacksResponse = ListAssociatedStacksResponse'
  { _lasrsNextToken      :: !(Maybe Text)
  , _lasrsNames          :: !(Maybe [Text])
  , _lasrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAssociatedStacksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lasrsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'lasrsNames' - The names of the stacks.
--
-- * 'lasrsResponseStatus' - -- | The response status code.
listAssociatedStacksResponse
    :: Int -- ^ 'lasrsResponseStatus'
    -> ListAssociatedStacksResponse
listAssociatedStacksResponse pResponseStatus_ =
  ListAssociatedStacksResponse'
    { _lasrsNextToken = Nothing
    , _lasrsNames = Nothing
    , _lasrsResponseStatus = pResponseStatus_
    }


-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
lasrsNextToken :: Lens' ListAssociatedStacksResponse (Maybe Text)
lasrsNextToken = lens _lasrsNextToken (\ s a -> s{_lasrsNextToken = a})

-- | The names of the stacks.
lasrsNames :: Lens' ListAssociatedStacksResponse [Text]
lasrsNames = lens _lasrsNames (\ s a -> s{_lasrsNames = a}) . _Default . _Coerce

-- | -- | The response status code.
lasrsResponseStatus :: Lens' ListAssociatedStacksResponse Int
lasrsResponseStatus = lens _lasrsResponseStatus (\ s a -> s{_lasrsResponseStatus = a})

instance NFData ListAssociatedStacksResponse where
