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
-- Module      : Network.AWS.Lightsail.UpdateDomainEntry
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a domain recordset after it is created.
--
--
module Network.AWS.Lightsail.UpdateDomainEntry
    (
    -- * Creating a Request
      updateDomainEntry
    , UpdateDomainEntry
    -- * Request Lenses
    , udeDomainName
    , udeDomainEntry

    -- * Destructuring the Response
    , updateDomainEntryResponse
    , UpdateDomainEntryResponse
    -- * Response Lenses
    , udersOperations
    , udersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDomainEntry' smart constructor.
data UpdateDomainEntry = UpdateDomainEntry'
  { _udeDomainName  :: !Text
  , _udeDomainEntry :: !DomainEntry
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDomainEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udeDomainName' - The name of the domain recordset to update.
--
-- * 'udeDomainEntry' - An array of key-value pairs containing information about the domain entry.
updateDomainEntry
    :: Text -- ^ 'udeDomainName'
    -> DomainEntry -- ^ 'udeDomainEntry'
    -> UpdateDomainEntry
updateDomainEntry pDomainName_ pDomainEntry_ =
  UpdateDomainEntry'
    {_udeDomainName = pDomainName_, _udeDomainEntry = pDomainEntry_}


-- | The name of the domain recordset to update.
udeDomainName :: Lens' UpdateDomainEntry Text
udeDomainName = lens _udeDomainName (\ s a -> s{_udeDomainName = a})

-- | An array of key-value pairs containing information about the domain entry.
udeDomainEntry :: Lens' UpdateDomainEntry DomainEntry
udeDomainEntry = lens _udeDomainEntry (\ s a -> s{_udeDomainEntry = a})

instance AWSRequest UpdateDomainEntry where
        type Rs UpdateDomainEntry = UpdateDomainEntryResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDomainEntryResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable UpdateDomainEntry where

instance NFData UpdateDomainEntry where

instance ToHeaders UpdateDomainEntry where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.UpdateDomainEntry" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDomainEntry where
        toJSON UpdateDomainEntry'{..}
          = object
              (catMaybes
                 [Just ("domainName" .= _udeDomainName),
                  Just ("domainEntry" .= _udeDomainEntry)])

instance ToPath UpdateDomainEntry where
        toPath = const "/"

instance ToQuery UpdateDomainEntry where
        toQuery = const mempty

-- | /See:/ 'updateDomainEntryResponse' smart constructor.
data UpdateDomainEntryResponse = UpdateDomainEntryResponse'
  { _udersOperations     :: !(Maybe [Operation])
  , _udersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDomainEntryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udersOperations' - An array of key-value pairs containing information about the request operation.
--
-- * 'udersResponseStatus' - -- | The response status code.
updateDomainEntryResponse
    :: Int -- ^ 'udersResponseStatus'
    -> UpdateDomainEntryResponse
updateDomainEntryResponse pResponseStatus_ =
  UpdateDomainEntryResponse'
    {_udersOperations = Nothing, _udersResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the request operation.
udersOperations :: Lens' UpdateDomainEntryResponse [Operation]
udersOperations = lens _udersOperations (\ s a -> s{_udersOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
udersResponseStatus :: Lens' UpdateDomainEntryResponse Int
udersResponseStatus = lens _udersResponseStatus (\ s a -> s{_udersResponseStatus = a})

instance NFData UpdateDomainEntryResponse where
