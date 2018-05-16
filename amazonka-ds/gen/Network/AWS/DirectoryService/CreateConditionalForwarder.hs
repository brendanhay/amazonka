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
-- Module      : Network.AWS.DirectoryService.CreateConditionalForwarder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a conditional forwarder associated with your AWS directory. Conditional forwarders are required in order to set up a trust relationship with another domain. The conditional forwarder points to the trusted domain.
--
--
module Network.AWS.DirectoryService.CreateConditionalForwarder
    (
    -- * Creating a Request
      createConditionalForwarder
    , CreateConditionalForwarder
    -- * Request Lenses
    , ccfDirectoryId
    , ccfRemoteDomainName
    , ccfDNSIPAddrs

    -- * Destructuring the Response
    , createConditionalForwarderResponse
    , CreateConditionalForwarderResponse
    -- * Response Lenses
    , ccfrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Initiates the creation of a conditional forwarder for your AWS Directory Service for Microsoft Active Directory. Conditional forwarders are required in order to set up a trust relationship with another domain.
--
--
--
-- /See:/ 'createConditionalForwarder' smart constructor.
data CreateConditionalForwarder = CreateConditionalForwarder'
  { _ccfDirectoryId      :: !Text
  , _ccfRemoteDomainName :: !Text
  , _ccfDNSIPAddrs       :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConditionalForwarder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfDirectoryId' - The directory ID of the AWS directory for which you are creating the conditional forwarder.
--
-- * 'ccfRemoteDomainName' - The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
--
-- * 'ccfDNSIPAddrs' - The IP addresses of the remote DNS server associated with RemoteDomainName.
createConditionalForwarder
    :: Text -- ^ 'ccfDirectoryId'
    -> Text -- ^ 'ccfRemoteDomainName'
    -> CreateConditionalForwarder
createConditionalForwarder pDirectoryId_ pRemoteDomainName_ =
  CreateConditionalForwarder'
    { _ccfDirectoryId = pDirectoryId_
    , _ccfRemoteDomainName = pRemoteDomainName_
    , _ccfDNSIPAddrs = mempty
    }


-- | The directory ID of the AWS directory for which you are creating the conditional forwarder.
ccfDirectoryId :: Lens' CreateConditionalForwarder Text
ccfDirectoryId = lens _ccfDirectoryId (\ s a -> s{_ccfDirectoryId = a})

-- | The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
ccfRemoteDomainName :: Lens' CreateConditionalForwarder Text
ccfRemoteDomainName = lens _ccfRemoteDomainName (\ s a -> s{_ccfRemoteDomainName = a})

-- | The IP addresses of the remote DNS server associated with RemoteDomainName.
ccfDNSIPAddrs :: Lens' CreateConditionalForwarder [Text]
ccfDNSIPAddrs = lens _ccfDNSIPAddrs (\ s a -> s{_ccfDNSIPAddrs = a}) . _Coerce

instance AWSRequest CreateConditionalForwarder where
        type Rs CreateConditionalForwarder =
             CreateConditionalForwarderResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 CreateConditionalForwarderResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateConditionalForwarder where

instance NFData CreateConditionalForwarder where

instance ToHeaders CreateConditionalForwarder where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.CreateConditionalForwarder"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateConditionalForwarder where
        toJSON CreateConditionalForwarder'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _ccfDirectoryId),
                  Just ("RemoteDomainName" .= _ccfRemoteDomainName),
                  Just ("DnsIpAddrs" .= _ccfDNSIPAddrs)])

instance ToPath CreateConditionalForwarder where
        toPath = const "/"

instance ToQuery CreateConditionalForwarder where
        toQuery = const mempty

-- | The result of a CreateConditinalForwarder request.
--
--
--
-- /See:/ 'createConditionalForwarderResponse' smart constructor.
newtype CreateConditionalForwarderResponse = CreateConditionalForwarderResponse'
  { _ccfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConditionalForwarderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfrsResponseStatus' - -- | The response status code.
createConditionalForwarderResponse
    :: Int -- ^ 'ccfrsResponseStatus'
    -> CreateConditionalForwarderResponse
createConditionalForwarderResponse pResponseStatus_ =
  CreateConditionalForwarderResponse' {_ccfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ccfrsResponseStatus :: Lens' CreateConditionalForwarderResponse Int
ccfrsResponseStatus = lens _ccfrsResponseStatus (\ s a -> s{_ccfrsResponseStatus = a})

instance NFData CreateConditionalForwarderResponse
         where
