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
-- Module      : Network.AWS.DirectoryService.DeleteConditionalForwarder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a conditional forwarder that has been set up for your AWS directory.
--
--
module Network.AWS.DirectoryService.DeleteConditionalForwarder
    (
    -- * Creating a Request
      deleteConditionalForwarder
    , DeleteConditionalForwarder
    -- * Request Lenses
    , delDirectoryId
    , delRemoteDomainName

    -- * Destructuring the Response
    , deleteConditionalForwarderResponse
    , DeleteConditionalForwarderResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Deletes a conditional forwarder.
--
--
--
-- /See:/ 'deleteConditionalForwarder' smart constructor.
data DeleteConditionalForwarder = DeleteConditionalForwarder'
  { _delDirectoryId      :: !Text
  , _delRemoteDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConditionalForwarder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delDirectoryId' - The directory ID for which you are deleting the conditional forwarder.
--
-- * 'delRemoteDomainName' - The fully qualified domain name (FQDN) of the remote domain with which you are deleting the conditional forwarder.
deleteConditionalForwarder
    :: Text -- ^ 'delDirectoryId'
    -> Text -- ^ 'delRemoteDomainName'
    -> DeleteConditionalForwarder
deleteConditionalForwarder pDirectoryId_ pRemoteDomainName_ =
  DeleteConditionalForwarder'
    {_delDirectoryId = pDirectoryId_, _delRemoteDomainName = pRemoteDomainName_}


-- | The directory ID for which you are deleting the conditional forwarder.
delDirectoryId :: Lens' DeleteConditionalForwarder Text
delDirectoryId = lens _delDirectoryId (\ s a -> s{_delDirectoryId = a})

-- | The fully qualified domain name (FQDN) of the remote domain with which you are deleting the conditional forwarder.
delRemoteDomainName :: Lens' DeleteConditionalForwarder Text
delRemoteDomainName = lens _delRemoteDomainName (\ s a -> s{_delRemoteDomainName = a})

instance AWSRequest DeleteConditionalForwarder where
        type Rs DeleteConditionalForwarder =
             DeleteConditionalForwarderResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteConditionalForwarderResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteConditionalForwarder where

instance NFData DeleteConditionalForwarder where

instance ToHeaders DeleteConditionalForwarder where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DeleteConditionalForwarder"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteConditionalForwarder where
        toJSON DeleteConditionalForwarder'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _delDirectoryId),
                  Just ("RemoteDomainName" .= _delRemoteDomainName)])

instance ToPath DeleteConditionalForwarder where
        toPath = const "/"

instance ToQuery DeleteConditionalForwarder where
        toQuery = const mempty

-- | The result of a DeleteConditionalForwarder request.
--
--
--
-- /See:/ 'deleteConditionalForwarderResponse' smart constructor.
newtype DeleteConditionalForwarderResponse = DeleteConditionalForwarderResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConditionalForwarderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteConditionalForwarderResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteConditionalForwarderResponse
deleteConditionalForwarderResponse pResponseStatus_ =
  DeleteConditionalForwarderResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteConditionalForwarderResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteConditionalForwarderResponse
         where
