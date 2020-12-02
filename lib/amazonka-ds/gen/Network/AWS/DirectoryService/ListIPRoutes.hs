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
-- Module      : Network.AWS.DirectoryService.ListIPRoutes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the address blocks that you have added to a directory.
--
--
module Network.AWS.DirectoryService.ListIPRoutes
    (
    -- * Creating a Request
      listIPRoutes
    , ListIPRoutes
    -- * Request Lenses
    , lirNextToken
    , lirLimit
    , lirDirectoryId

    -- * Destructuring the Response
    , listIPRoutesResponse
    , ListIPRoutesResponse
    -- * Response Lenses
    , lirrsIPRoutesInfo
    , lirrsNextToken
    , lirrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listIPRoutes' smart constructor.
data ListIPRoutes = ListIPRoutes'
  { _lirNextToken   :: !(Maybe Text)
  , _lirLimit       :: !(Maybe Nat)
  , _lirDirectoryId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIPRoutes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirNextToken' - The /ListIpRoutes.NextToken/ value from a previous call to 'ListIpRoutes' . Pass null if this is the first call.
--
-- * 'lirLimit' - Maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
--
-- * 'lirDirectoryId' - Identifier (ID) of the directory for which you want to retrieve the IP addresses.
listIPRoutes
    :: Text -- ^ 'lirDirectoryId'
    -> ListIPRoutes
listIPRoutes pDirectoryId_ =
  ListIPRoutes'
    { _lirNextToken = Nothing
    , _lirLimit = Nothing
    , _lirDirectoryId = pDirectoryId_
    }


-- | The /ListIpRoutes.NextToken/ value from a previous call to 'ListIpRoutes' . Pass null if this is the first call.
lirNextToken :: Lens' ListIPRoutes (Maybe Text)
lirNextToken = lens _lirNextToken (\ s a -> s{_lirNextToken = a})

-- | Maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
lirLimit :: Lens' ListIPRoutes (Maybe Natural)
lirLimit = lens _lirLimit (\ s a -> s{_lirLimit = a}) . mapping _Nat

-- | Identifier (ID) of the directory for which you want to retrieve the IP addresses.
lirDirectoryId :: Lens' ListIPRoutes Text
lirDirectoryId = lens _lirDirectoryId (\ s a -> s{_lirDirectoryId = a})

instance AWSRequest ListIPRoutes where
        type Rs ListIPRoutes = ListIPRoutesResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 ListIPRoutesResponse' <$>
                   (x .?> "IpRoutesInfo" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListIPRoutes where

instance NFData ListIPRoutes where

instance ToHeaders ListIPRoutes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.ListIpRoutes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListIPRoutes where
        toJSON ListIPRoutes'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lirNextToken,
                  ("Limit" .=) <$> _lirLimit,
                  Just ("DirectoryId" .= _lirDirectoryId)])

instance ToPath ListIPRoutes where
        toPath = const "/"

instance ToQuery ListIPRoutes where
        toQuery = const mempty

-- | /See:/ 'listIPRoutesResponse' smart constructor.
data ListIPRoutesResponse = ListIPRoutesResponse'
  { _lirrsIPRoutesInfo   :: !(Maybe [IPRouteInfo])
  , _lirrsNextToken      :: !(Maybe Text)
  , _lirrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIPRoutesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirrsIPRoutesInfo' - A list of 'IpRoute' s.
--
-- * 'lirrsNextToken' - If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'ListIpRoutes' to retrieve the next set of items.
--
-- * 'lirrsResponseStatus' - -- | The response status code.
listIPRoutesResponse
    :: Int -- ^ 'lirrsResponseStatus'
    -> ListIPRoutesResponse
listIPRoutesResponse pResponseStatus_ =
  ListIPRoutesResponse'
    { _lirrsIPRoutesInfo = Nothing
    , _lirrsNextToken = Nothing
    , _lirrsResponseStatus = pResponseStatus_
    }


-- | A list of 'IpRoute' s.
lirrsIPRoutesInfo :: Lens' ListIPRoutesResponse [IPRouteInfo]
lirrsIPRoutesInfo = lens _lirrsIPRoutesInfo (\ s a -> s{_lirrsIPRoutesInfo = a}) . _Default . _Coerce

-- | If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'ListIpRoutes' to retrieve the next set of items.
lirrsNextToken :: Lens' ListIPRoutesResponse (Maybe Text)
lirrsNextToken = lens _lirrsNextToken (\ s a -> s{_lirrsNextToken = a})

-- | -- | The response status code.
lirrsResponseStatus :: Lens' ListIPRoutesResponse Int
lirrsResponseStatus = lens _lirrsResponseStatus (\ s a -> s{_lirrsResponseStatus = a})

instance NFData ListIPRoutesResponse where
