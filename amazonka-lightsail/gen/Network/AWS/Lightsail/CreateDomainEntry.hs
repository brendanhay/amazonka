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
-- Module      : Network.AWS.Lightsail.CreateDomainEntry
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one of the following entry records associated with the domain: A record, CNAME record, TXT record, or MX record.
--
--
module Network.AWS.Lightsail.CreateDomainEntry
    (
    -- * Creating a Request
      createDomainEntry
    , CreateDomainEntry
    -- * Request Lenses
    , cdeDomainName
    , cdeDomainEntry

    -- * Destructuring the Response
    , createDomainEntryResponse
    , CreateDomainEntryResponse
    -- * Response Lenses
    , cdersOperation
    , cdersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDomainEntry' smart constructor.
data CreateDomainEntry = CreateDomainEntry'
  { _cdeDomainName  :: !Text
  , _cdeDomainEntry :: !DomainEntry
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDomainEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdeDomainName' - The domain name (e.g., @example.com@ ) for which you want to create the domain entry.
--
-- * 'cdeDomainEntry' - An array of key-value pairs containing information about the domain entry request.
createDomainEntry
    :: Text -- ^ 'cdeDomainName'
    -> DomainEntry -- ^ 'cdeDomainEntry'
    -> CreateDomainEntry
createDomainEntry pDomainName_ pDomainEntry_ =
  CreateDomainEntry'
    {_cdeDomainName = pDomainName_, _cdeDomainEntry = pDomainEntry_}


-- | The domain name (e.g., @example.com@ ) for which you want to create the domain entry.
cdeDomainName :: Lens' CreateDomainEntry Text
cdeDomainName = lens _cdeDomainName (\ s a -> s{_cdeDomainName = a})

-- | An array of key-value pairs containing information about the domain entry request.
cdeDomainEntry :: Lens' CreateDomainEntry DomainEntry
cdeDomainEntry = lens _cdeDomainEntry (\ s a -> s{_cdeDomainEntry = a})

instance AWSRequest CreateDomainEntry where
        type Rs CreateDomainEntry = CreateDomainEntryResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 CreateDomainEntryResponse' <$>
                   (x .?> "operation") <*> (pure (fromEnum s)))

instance Hashable CreateDomainEntry where

instance NFData CreateDomainEntry where

instance ToHeaders CreateDomainEntry where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.CreateDomainEntry" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDomainEntry where
        toJSON CreateDomainEntry'{..}
          = object
              (catMaybes
                 [Just ("domainName" .= _cdeDomainName),
                  Just ("domainEntry" .= _cdeDomainEntry)])

instance ToPath CreateDomainEntry where
        toPath = const "/"

instance ToQuery CreateDomainEntry where
        toQuery = const mempty

-- | /See:/ 'createDomainEntryResponse' smart constructor.
data CreateDomainEntryResponse = CreateDomainEntryResponse'
  { _cdersOperation      :: !(Maybe Operation)
  , _cdersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDomainEntryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdersOperation' - An array of key-value pairs containing information about the operation.
--
-- * 'cdersResponseStatus' - -- | The response status code.
createDomainEntryResponse
    :: Int -- ^ 'cdersResponseStatus'
    -> CreateDomainEntryResponse
createDomainEntryResponse pResponseStatus_ =
  CreateDomainEntryResponse'
    {_cdersOperation = Nothing, _cdersResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the operation.
cdersOperation :: Lens' CreateDomainEntryResponse (Maybe Operation)
cdersOperation = lens _cdersOperation (\ s a -> s{_cdersOperation = a})

-- | -- | The response status code.
cdersResponseStatus :: Lens' CreateDomainEntryResponse Int
cdersResponseStatus = lens _cdersResponseStatus (\ s a -> s{_cdersResponseStatus = a})

instance NFData CreateDomainEntryResponse where
