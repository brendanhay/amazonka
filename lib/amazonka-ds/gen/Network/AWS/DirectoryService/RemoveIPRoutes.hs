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
-- Module      : Network.AWS.DirectoryService.RemoveIPRoutes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes IP address blocks from a directory.
--
--
module Network.AWS.DirectoryService.RemoveIPRoutes
    (
    -- * Creating a Request
      removeIPRoutes
    , RemoveIPRoutes
    -- * Request Lenses
    , rirDirectoryId
    , rirCidrIPs

    -- * Destructuring the Response
    , removeIPRoutesResponse
    , RemoveIPRoutesResponse
    -- * Response Lenses
    , rirrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeIPRoutes' smart constructor.
data RemoveIPRoutes = RemoveIPRoutes'
  { _rirDirectoryId :: !Text
  , _rirCidrIPs     :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveIPRoutes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirDirectoryId' - Identifier (ID) of the directory from which you want to remove the IP addresses.
--
-- * 'rirCidrIPs' - IP address blocks that you want to remove.
removeIPRoutes
    :: Text -- ^ 'rirDirectoryId'
    -> RemoveIPRoutes
removeIPRoutes pDirectoryId_ =
  RemoveIPRoutes' {_rirDirectoryId = pDirectoryId_, _rirCidrIPs = mempty}


-- | Identifier (ID) of the directory from which you want to remove the IP addresses.
rirDirectoryId :: Lens' RemoveIPRoutes Text
rirDirectoryId = lens _rirDirectoryId (\ s a -> s{_rirDirectoryId = a})

-- | IP address blocks that you want to remove.
rirCidrIPs :: Lens' RemoveIPRoutes [Text]
rirCidrIPs = lens _rirCidrIPs (\ s a -> s{_rirCidrIPs = a}) . _Coerce

instance AWSRequest RemoveIPRoutes where
        type Rs RemoveIPRoutes = RemoveIPRoutesResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 RemoveIPRoutesResponse' <$> (pure (fromEnum s)))

instance Hashable RemoveIPRoutes where

instance NFData RemoveIPRoutes where

instance ToHeaders RemoveIPRoutes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.RemoveIpRoutes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveIPRoutes where
        toJSON RemoveIPRoutes'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _rirDirectoryId),
                  Just ("CidrIps" .= _rirCidrIPs)])

instance ToPath RemoveIPRoutes where
        toPath = const "/"

instance ToQuery RemoveIPRoutes where
        toQuery = const mempty

-- | /See:/ 'removeIPRoutesResponse' smart constructor.
newtype RemoveIPRoutesResponse = RemoveIPRoutesResponse'
  { _rirrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveIPRoutesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirrsResponseStatus' - -- | The response status code.
removeIPRoutesResponse
    :: Int -- ^ 'rirrsResponseStatus'
    -> RemoveIPRoutesResponse
removeIPRoutesResponse pResponseStatus_ =
  RemoveIPRoutesResponse' {_rirrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rirrsResponseStatus :: Lens' RemoveIPRoutesResponse Int
rirrsResponseStatus = lens _rirrsResponseStatus (\ s a -> s{_rirrsResponseStatus = a})

instance NFData RemoveIPRoutesResponse where
