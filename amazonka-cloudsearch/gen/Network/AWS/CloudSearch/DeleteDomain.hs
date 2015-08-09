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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a search domain and all of its data. Once a domain
-- has been deleted, it cannot be recovered. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/deleting-domains.html Deleting a Search Domain>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DeleteDomain.html AWS API Reference> for DeleteDomain.
module Network.AWS.CloudSearch.DeleteDomain
    (
    -- * Creating a Request
      DeleteDomain
    , deleteDomain
    -- * Request Lenses
    , dddDomainName

    -- * Destructuring the Response
    , DeleteDomainResponse
    , deleteDomainResponse
    -- * Response Lenses
    , ddrsDomainStatus
    , ddrsStatus
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @DeleteDomain@ operation. Specifies
-- the name of the domain you want to delete.
--
-- /See:/ 'deleteDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dddDomainName'
newtype DeleteDomain = DeleteDomain'
    { _dddDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDomain' smart constructor.
deleteDomain :: Text -> DeleteDomain
deleteDomain pDomainName_ = 
    DeleteDomain'
    { _dddDomainName = pDomainName_
    }

-- | The name of the domain you want to permanently delete.
dddDomainName :: Lens' DeleteDomain Text
dddDomainName = lens _dddDomainName (\ s a -> s{_dddDomainName = a});

instance AWSRequest DeleteDomain where
        type Sv DeleteDomain = CloudSearch
        type Rs DeleteDomain = DeleteDomainResponse
        request = postQuery
        response
          = receiveXMLWrapper "DeleteDomainResult"
              (\ s h x ->
                 DeleteDomainResponse' <$>
                   (x .@? "DomainStatus") <*> (pure (fromEnum s)))

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

-- | The result of a @DeleteDomain@ request. Contains the status of a newly
-- deleted domain, or no status if the domain has already been completely
-- deleted.
--
-- /See:/ 'deleteDomainResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrsDomainStatus'
--
-- * 'ddrsStatus'
data DeleteDomainResponse = DeleteDomainResponse'
    { _ddrsDomainStatus :: !(Maybe DomainStatus)
    , _ddrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDomainResponse' smart constructor.
deleteDomainResponse :: Int -> DeleteDomainResponse
deleteDomainResponse pStatus_ = 
    DeleteDomainResponse'
    { _ddrsDomainStatus = Nothing
    , _ddrsStatus = pStatus_
    }

-- | Undocumented member.
ddrsDomainStatus :: Lens' DeleteDomainResponse (Maybe DomainStatus)
ddrsDomainStatus = lens _ddrsDomainStatus (\ s a -> s{_ddrsDomainStatus = a});

-- | Undocumented member.
ddrsStatus :: Lens' DeleteDomainResponse Int
ddrsStatus = lens _ddrsStatus (\ s a -> s{_ddrsStatus = a});
