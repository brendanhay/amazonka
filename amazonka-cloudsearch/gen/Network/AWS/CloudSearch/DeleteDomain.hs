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
-- Module      : Network.AWS.CloudSearch.DeleteDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a search domain and all of its data. Once a domain has been deleted, it cannot be recovered. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/deleting-domains.html Deleting a Search Domain> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.DeleteDomain
    (
    -- * Creating a Request
      deleteDomain
    , DeleteDomain
    -- * Request Lenses
    , dddDomainName

    -- * Destructuring the Response
    , deleteDomainResponse
    , DeleteDomainResponse
    -- * Response Lenses
    , ddrsDomainStatus
    , ddrsResponseStatus
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DeleteDomain' @ operation. Specifies the name of the domain you want to delete.
--
--
--
-- /See:/ 'deleteDomain' smart constructor.
newtype DeleteDomain = DeleteDomain'
  { _dddDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dddDomainName' - The name of the domain you want to permanently delete.
deleteDomain
    :: Text -- ^ 'dddDomainName'
    -> DeleteDomain
deleteDomain pDomainName_ = DeleteDomain' {_dddDomainName = pDomainName_}


-- | The name of the domain you want to permanently delete.
dddDomainName :: Lens' DeleteDomain Text
dddDomainName = lens _dddDomainName (\ s a -> s{_dddDomainName = a})

instance AWSRequest DeleteDomain where
        type Rs DeleteDomain = DeleteDomainResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "DeleteDomainResult"
              (\ s h x ->
                 DeleteDomainResponse' <$>
                   (x .@? "DomainStatus") <*> (pure (fromEnum s)))

instance Hashable DeleteDomain where

instance NFData DeleteDomain where

instance ToHeaders DeleteDomain where
        toHeaders = const mempty

instance ToPath DeleteDomain where
        toPath = const "/"

instance ToQuery DeleteDomain where
        toQuery DeleteDomain'{..}
          = mconcat
              ["Action" =: ("DeleteDomain" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _dddDomainName]

-- | The result of a @DeleteDomain@ request. Contains the status of a newly deleted domain, or no status if the domain has already been completely deleted.
--
--
--
-- /See:/ 'deleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  { _ddrsDomainStatus   :: !(Maybe DomainStatus)
  , _ddrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsDomainStatus' - Undocumented member.
--
-- * 'ddrsResponseStatus' - -- | The response status code.
deleteDomainResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DeleteDomainResponse
deleteDomainResponse pResponseStatus_ =
  DeleteDomainResponse'
    {_ddrsDomainStatus = Nothing, _ddrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
ddrsDomainStatus :: Lens' DeleteDomainResponse (Maybe DomainStatus)
ddrsDomainStatus = lens _ddrsDomainStatus (\ s a -> s{_ddrsDomainStatus = a})

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DeleteDomainResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

instance NFData DeleteDomainResponse where
