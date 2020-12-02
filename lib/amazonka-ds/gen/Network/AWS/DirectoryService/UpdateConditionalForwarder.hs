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
-- Module      : Network.AWS.DirectoryService.UpdateConditionalForwarder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a conditional forwarder that has been set up for your AWS directory.
--
--
module Network.AWS.DirectoryService.UpdateConditionalForwarder
    (
    -- * Creating a Request
      updateConditionalForwarder
    , UpdateConditionalForwarder
    -- * Request Lenses
    , ucfDirectoryId
    , ucfRemoteDomainName
    , ucfDNSIPAddrs

    -- * Destructuring the Response
    , updateConditionalForwarderResponse
    , UpdateConditionalForwarderResponse
    -- * Response Lenses
    , ucfrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Updates a conditional forwarder.
--
--
--
-- /See:/ 'updateConditionalForwarder' smart constructor.
data UpdateConditionalForwarder = UpdateConditionalForwarder'
  { _ucfDirectoryId      :: !Text
  , _ucfRemoteDomainName :: !Text
  , _ucfDNSIPAddrs       :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConditionalForwarder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucfDirectoryId' - The directory ID of the AWS directory for which to update the conditional forwarder.
--
-- * 'ucfRemoteDomainName' - The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
--
-- * 'ucfDNSIPAddrs' - The updated IP addresses of the remote DNS server associated with the conditional forwarder.
updateConditionalForwarder
    :: Text -- ^ 'ucfDirectoryId'
    -> Text -- ^ 'ucfRemoteDomainName'
    -> UpdateConditionalForwarder
updateConditionalForwarder pDirectoryId_ pRemoteDomainName_ =
  UpdateConditionalForwarder'
    { _ucfDirectoryId = pDirectoryId_
    , _ucfRemoteDomainName = pRemoteDomainName_
    , _ucfDNSIPAddrs = mempty
    }


-- | The directory ID of the AWS directory for which to update the conditional forwarder.
ucfDirectoryId :: Lens' UpdateConditionalForwarder Text
ucfDirectoryId = lens _ucfDirectoryId (\ s a -> s{_ucfDirectoryId = a})

-- | The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
ucfRemoteDomainName :: Lens' UpdateConditionalForwarder Text
ucfRemoteDomainName = lens _ucfRemoteDomainName (\ s a -> s{_ucfRemoteDomainName = a})

-- | The updated IP addresses of the remote DNS server associated with the conditional forwarder.
ucfDNSIPAddrs :: Lens' UpdateConditionalForwarder [Text]
ucfDNSIPAddrs = lens _ucfDNSIPAddrs (\ s a -> s{_ucfDNSIPAddrs = a}) . _Coerce

instance AWSRequest UpdateConditionalForwarder where
        type Rs UpdateConditionalForwarder =
             UpdateConditionalForwarderResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateConditionalForwarderResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateConditionalForwarder where

instance NFData UpdateConditionalForwarder where

instance ToHeaders UpdateConditionalForwarder where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.UpdateConditionalForwarder"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateConditionalForwarder where
        toJSON UpdateConditionalForwarder'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _ucfDirectoryId),
                  Just ("RemoteDomainName" .= _ucfRemoteDomainName),
                  Just ("DnsIpAddrs" .= _ucfDNSIPAddrs)])

instance ToPath UpdateConditionalForwarder where
        toPath = const "/"

instance ToQuery UpdateConditionalForwarder where
        toQuery = const mempty

-- | The result of an UpdateConditionalForwarder request.
--
--
--
-- /See:/ 'updateConditionalForwarderResponse' smart constructor.
newtype UpdateConditionalForwarderResponse = UpdateConditionalForwarderResponse'
  { _ucfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConditionalForwarderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucfrsResponseStatus' - -- | The response status code.
updateConditionalForwarderResponse
    :: Int -- ^ 'ucfrsResponseStatus'
    -> UpdateConditionalForwarderResponse
updateConditionalForwarderResponse pResponseStatus_ =
  UpdateConditionalForwarderResponse' {_ucfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ucfrsResponseStatus :: Lens' UpdateConditionalForwarderResponse Int
ucfrsResponseStatus = lens _ucfrsResponseStatus (\ s a -> s{_ucfrsResponseStatus = a})

instance NFData UpdateConditionalForwarderResponse
         where
