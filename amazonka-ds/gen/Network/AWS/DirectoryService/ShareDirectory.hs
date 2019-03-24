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
-- Module      : Network.AWS.DirectoryService.ShareDirectory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares a specified directory (@DirectoryId@ ) in your AWS account (directory owner) with another AWS account (directory consumer). With this operation you can use your directory from any AWS account and from any Amazon VPC within an AWS Region.
--
--
-- When you share your AWS Managed Microsoft AD directory, AWS Directory Service creates a shared directory in the directory consumer account. This shared directory contains the metadata to provide access to the directory within the directory owner account. The shared directory is visible in all VPCs in the directory consumer account.
--
-- The @ShareMethod@ parameter determines whether the specified directory can be shared between AWS accounts inside the same AWS organization (@ORGANIZATIONS@ ). It also determines whether you can share the directory with any other AWS account either inside or outside of the organization (@HANDSHAKE@ ).
--
-- The @ShareNotes@ parameter is only used when @HANDSHAKE@ is called, which sends a directory sharing request to the directory consumer.
--
module Network.AWS.DirectoryService.ShareDirectory
    (
    -- * Creating a Request
      shareDirectory
    , ShareDirectory
    -- * Request Lenses
    , sdShareNotes
    , sdDirectoryId
    , sdShareTarget
    , sdShareMethod

    -- * Destructuring the Response
    , shareDirectoryResponse
    , ShareDirectoryResponse
    -- * Response Lenses
    , sdrsSharedDirectoryId
    , sdrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'shareDirectory' smart constructor.
data ShareDirectory = ShareDirectory'
  { _sdShareNotes  :: !(Maybe (Sensitive Text))
  , _sdDirectoryId :: !Text
  , _sdShareTarget :: !ShareTarget
  , _sdShareMethod :: !ShareMethod
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ShareDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdShareNotes' - A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
--
-- * 'sdDirectoryId' - Identifier of the AWS Managed Microsoft AD directory that you want to share with other AWS accounts.
--
-- * 'sdShareTarget' - Identifier for the directory consumer account with whom the directory is to be shared.
--
-- * 'sdShareMethod' - The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a directory sharing request (@HANDSHAKE@ ).
shareDirectory
    :: Text -- ^ 'sdDirectoryId'
    -> ShareTarget -- ^ 'sdShareTarget'
    -> ShareMethod -- ^ 'sdShareMethod'
    -> ShareDirectory
shareDirectory pDirectoryId_ pShareTarget_ pShareMethod_ =
  ShareDirectory'
    { _sdShareNotes = Nothing
    , _sdDirectoryId = pDirectoryId_
    , _sdShareTarget = pShareTarget_
    , _sdShareMethod = pShareMethod_
    }


-- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
sdShareNotes :: Lens' ShareDirectory (Maybe Text)
sdShareNotes = lens _sdShareNotes (\ s a -> s{_sdShareNotes = a}) . mapping _Sensitive

-- | Identifier of the AWS Managed Microsoft AD directory that you want to share with other AWS accounts.
sdDirectoryId :: Lens' ShareDirectory Text
sdDirectoryId = lens _sdDirectoryId (\ s a -> s{_sdDirectoryId = a})

-- | Identifier for the directory consumer account with whom the directory is to be shared.
sdShareTarget :: Lens' ShareDirectory ShareTarget
sdShareTarget = lens _sdShareTarget (\ s a -> s{_sdShareTarget = a})

-- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a directory sharing request (@HANDSHAKE@ ).
sdShareMethod :: Lens' ShareDirectory ShareMethod
sdShareMethod = lens _sdShareMethod (\ s a -> s{_sdShareMethod = a})

instance AWSRequest ShareDirectory where
        type Rs ShareDirectory = ShareDirectoryResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 ShareDirectoryResponse' <$>
                   (x .?> "SharedDirectoryId") <*> (pure (fromEnum s)))

instance Hashable ShareDirectory where

instance NFData ShareDirectory where

instance ToHeaders ShareDirectory where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.ShareDirectory" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ShareDirectory where
        toJSON ShareDirectory'{..}
          = object
              (catMaybes
                 [("ShareNotes" .=) <$> _sdShareNotes,
                  Just ("DirectoryId" .= _sdDirectoryId),
                  Just ("ShareTarget" .= _sdShareTarget),
                  Just ("ShareMethod" .= _sdShareMethod)])

instance ToPath ShareDirectory where
        toPath = const "/"

instance ToQuery ShareDirectory where
        toQuery = const mempty

-- | /See:/ 'shareDirectoryResponse' smart constructor.
data ShareDirectoryResponse = ShareDirectoryResponse'
  { _sdrsSharedDirectoryId :: !(Maybe Text)
  , _sdrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ShareDirectoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdrsSharedDirectoryId' - Identifier of the directory that is stored in the directory consumer account that is shared from the specified directory (@DirectoryId@ ).
--
-- * 'sdrsResponseStatus' - -- | The response status code.
shareDirectoryResponse
    :: Int -- ^ 'sdrsResponseStatus'
    -> ShareDirectoryResponse
shareDirectoryResponse pResponseStatus_ =
  ShareDirectoryResponse'
    {_sdrsSharedDirectoryId = Nothing, _sdrsResponseStatus = pResponseStatus_}


-- | Identifier of the directory that is stored in the directory consumer account that is shared from the specified directory (@DirectoryId@ ).
sdrsSharedDirectoryId :: Lens' ShareDirectoryResponse (Maybe Text)
sdrsSharedDirectoryId = lens _sdrsSharedDirectoryId (\ s a -> s{_sdrsSharedDirectoryId = a})

-- | -- | The response status code.
sdrsResponseStatus :: Lens' ShareDirectoryResponse Int
sdrsResponseStatus = lens _sdrsResponseStatus (\ s a -> s{_sdrsResponseStatus = a})

instance NFData ShareDirectoryResponse where
