{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudSearch.DeleteDomain
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Permanently deletes a search domain and all of its data. Once a domain
-- has been deleted, it cannot be recovered. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/deleting-domains.html Deleting a Search Domain>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DeleteDomain.html>
module Network.AWS.CloudSearch.DeleteDomain
    (
    -- * Request
      DeleteDomain
    -- ** Request constructor
    , deleteDomain
    -- ** Request lenses
    , ddDomainName

    -- * Response
    , DeleteDomainResponse
    -- ** Response constructor
    , deleteDomainResponse
    -- ** Response lenses
    , ddrDomainStatus
    , ddrStatusCode
    ) where

import Network.AWS.CloudSearch.Types
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
-- * 'ddDomainName'
newtype DeleteDomain = DeleteDomain'{_ddDomainName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteDomain' smart constructor.
deleteDomain :: Text -> DeleteDomain
deleteDomain pDomainName = DeleteDomain'{_ddDomainName = pDomainName};

-- | The name of the domain you want to permanently delete.
ddDomainName :: Lens' DeleteDomain Text
ddDomainName = lens _ddDomainName (\ s a -> s{_ddDomainName = a});

instance AWSRequest DeleteDomain where
        type Sv DeleteDomain = CloudSearch
        type Rs DeleteDomain = DeleteDomainResponse
        request = post
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
               "DomainName" =: _ddDomainName]

-- | The result of a @DeleteDomain@ request. Contains the status of a newly
-- deleted domain, or no status if the domain has already been completely
-- deleted.
--
-- /See:/ 'deleteDomainResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrDomainStatus'
--
-- * 'ddrStatusCode'
data DeleteDomainResponse = DeleteDomainResponse'{_ddrDomainStatus :: Maybe DomainStatus, _ddrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DeleteDomainResponse' smart constructor.
deleteDomainResponse :: Int -> DeleteDomainResponse
deleteDomainResponse pStatusCode = DeleteDomainResponse'{_ddrDomainStatus = Nothing, _ddrStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
ddrDomainStatus :: Lens' DeleteDomainResponse (Maybe DomainStatus)
ddrDomainStatus = lens _ddrDomainStatus (\ s a -> s{_ddrDomainStatus = a});

-- | FIXME: Undocumented member.
ddrStatusCode :: Lens' DeleteDomainResponse Int
ddrStatusCode = lens _ddrStatusCode (\ s a -> s{_ddrStatusCode = a});
