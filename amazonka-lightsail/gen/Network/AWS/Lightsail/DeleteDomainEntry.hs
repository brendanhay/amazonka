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
-- Module      : Network.AWS.Lightsail.DeleteDomainEntry
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific domain entry.
--
--
module Network.AWS.Lightsail.DeleteDomainEntry
    (
    -- * Creating a Request
      deleteDomainEntry
    , DeleteDomainEntry
    -- * Request Lenses
    , ddeDomainName
    , ddeDomainEntry

    -- * Destructuring the Response
    , deleteDomainEntryResponse
    , DeleteDomainEntryResponse
    -- * Response Lenses
    , ddersOperation
    , ddersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDomainEntry' smart constructor.
data DeleteDomainEntry = DeleteDomainEntry'
  { _ddeDomainName  :: !Text
  , _ddeDomainEntry :: !DomainEntry
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDomainEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddeDomainName' - The name of the domain entry to delete.
--
-- * 'ddeDomainEntry' - An array of key-value pairs containing information about your domain entries.
deleteDomainEntry
    :: Text -- ^ 'ddeDomainName'
    -> DomainEntry -- ^ 'ddeDomainEntry'
    -> DeleteDomainEntry
deleteDomainEntry pDomainName_ pDomainEntry_ =
  DeleteDomainEntry'
    {_ddeDomainName = pDomainName_, _ddeDomainEntry = pDomainEntry_}


-- | The name of the domain entry to delete.
ddeDomainName :: Lens' DeleteDomainEntry Text
ddeDomainName = lens _ddeDomainName (\ s a -> s{_ddeDomainName = a})

-- | An array of key-value pairs containing information about your domain entries.
ddeDomainEntry :: Lens' DeleteDomainEntry DomainEntry
ddeDomainEntry = lens _ddeDomainEntry (\ s a -> s{_ddeDomainEntry = a})

instance AWSRequest DeleteDomainEntry where
        type Rs DeleteDomainEntry = DeleteDomainEntryResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDomainEntryResponse' <$>
                   (x .?> "operation") <*> (pure (fromEnum s)))

instance Hashable DeleteDomainEntry where

instance NFData DeleteDomainEntry where

instance ToHeaders DeleteDomainEntry where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.DeleteDomainEntry" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDomainEntry where
        toJSON DeleteDomainEntry'{..}
          = object
              (catMaybes
                 [Just ("domainName" .= _ddeDomainName),
                  Just ("domainEntry" .= _ddeDomainEntry)])

instance ToPath DeleteDomainEntry where
        toPath = const "/"

instance ToQuery DeleteDomainEntry where
        toQuery = const mempty

-- | /See:/ 'deleteDomainEntryResponse' smart constructor.
data DeleteDomainEntryResponse = DeleteDomainEntryResponse'
  { _ddersOperation      :: !(Maybe Operation)
  , _ddersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDomainEntryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddersOperation' - An array of key-value pairs containing information about the results of your delete domain entry request.
--
-- * 'ddersResponseStatus' - -- | The response status code.
deleteDomainEntryResponse
    :: Int -- ^ 'ddersResponseStatus'
    -> DeleteDomainEntryResponse
deleteDomainEntryResponse pResponseStatus_ =
  DeleteDomainEntryResponse'
    {_ddersOperation = Nothing, _ddersResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the results of your delete domain entry request.
ddersOperation :: Lens' DeleteDomainEntryResponse (Maybe Operation)
ddersOperation = lens _ddersOperation (\ s a -> s{_ddersOperation = a})

-- | -- | The response status code.
ddersResponseStatus :: Lens' DeleteDomainEntryResponse Int
ddersResponseStatus = lens _ddersResponseStatus (\ s a -> s{_ddersResponseStatus = a})

instance NFData DeleteDomainEntryResponse where
