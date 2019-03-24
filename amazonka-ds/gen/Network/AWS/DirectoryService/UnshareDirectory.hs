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
-- Module      : Network.AWS.DirectoryService.UnshareDirectory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the directory sharing between the directory owner and consumer accounts.
--
--
module Network.AWS.DirectoryService.UnshareDirectory
    (
    -- * Creating a Request
      unshareDirectory
    , UnshareDirectory
    -- * Request Lenses
    , udDirectoryId
    , udUnshareTarget

    -- * Destructuring the Response
    , unshareDirectoryResponse
    , UnshareDirectoryResponse
    -- * Response Lenses
    , udrsSharedDirectoryId
    , udrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'unshareDirectory' smart constructor.
data UnshareDirectory = UnshareDirectory'
  { _udDirectoryId   :: !Text
  , _udUnshareTarget :: !UnshareTarget
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnshareDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udDirectoryId' - The identifier of the AWS Managed Microsoft AD directory that you want to stop sharing.
--
-- * 'udUnshareTarget' - Identifier for the directory consumer account with whom the directory has to be unshared.
unshareDirectory
    :: Text -- ^ 'udDirectoryId'
    -> UnshareTarget -- ^ 'udUnshareTarget'
    -> UnshareDirectory
unshareDirectory pDirectoryId_ pUnshareTarget_ =
  UnshareDirectory'
    {_udDirectoryId = pDirectoryId_, _udUnshareTarget = pUnshareTarget_}


-- | The identifier of the AWS Managed Microsoft AD directory that you want to stop sharing.
udDirectoryId :: Lens' UnshareDirectory Text
udDirectoryId = lens _udDirectoryId (\ s a -> s{_udDirectoryId = a})

-- | Identifier for the directory consumer account with whom the directory has to be unshared.
udUnshareTarget :: Lens' UnshareDirectory UnshareTarget
udUnshareTarget = lens _udUnshareTarget (\ s a -> s{_udUnshareTarget = a})

instance AWSRequest UnshareDirectory where
        type Rs UnshareDirectory = UnshareDirectoryResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 UnshareDirectoryResponse' <$>
                   (x .?> "SharedDirectoryId") <*> (pure (fromEnum s)))

instance Hashable UnshareDirectory where

instance NFData UnshareDirectory where

instance ToHeaders UnshareDirectory where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.UnshareDirectory" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UnshareDirectory where
        toJSON UnshareDirectory'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _udDirectoryId),
                  Just ("UnshareTarget" .= _udUnshareTarget)])

instance ToPath UnshareDirectory where
        toPath = const "/"

instance ToQuery UnshareDirectory where
        toQuery = const mempty

-- | /See:/ 'unshareDirectoryResponse' smart constructor.
data UnshareDirectoryResponse = UnshareDirectoryResponse'
  { _udrsSharedDirectoryId :: !(Maybe Text)
  , _udrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnshareDirectoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrsSharedDirectoryId' - Identifier of the directory stored in the directory consumer account that is to be unshared from the specified directory (@DirectoryId@ ).
--
-- * 'udrsResponseStatus' - -- | The response status code.
unshareDirectoryResponse
    :: Int -- ^ 'udrsResponseStatus'
    -> UnshareDirectoryResponse
unshareDirectoryResponse pResponseStatus_ =
  UnshareDirectoryResponse'
    {_udrsSharedDirectoryId = Nothing, _udrsResponseStatus = pResponseStatus_}


-- | Identifier of the directory stored in the directory consumer account that is to be unshared from the specified directory (@DirectoryId@ ).
udrsSharedDirectoryId :: Lens' UnshareDirectoryResponse (Maybe Text)
udrsSharedDirectoryId = lens _udrsSharedDirectoryId (\ s a -> s{_udrsSharedDirectoryId = a})

-- | -- | The response status code.
udrsResponseStatus :: Lens' UnshareDirectoryResponse Int
udrsResponseStatus = lens _udrsResponseStatus (\ s a -> s{_udrsResponseStatus = a})

instance NFData UnshareDirectoryResponse where
