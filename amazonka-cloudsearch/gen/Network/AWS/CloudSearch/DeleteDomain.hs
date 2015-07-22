{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteDomain
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a search domain and all of its data. Once a domain
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
    , dddrqDomainName

    -- * Response
    , DeleteDomainResponse
    -- ** Response constructor
    , deleteDomainResponse
    -- ** Response lenses
    , dddrsDomainStatus
    , dddrsStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DeleteDomain@ operation. Specifies
-- the name of the domain you want to delete.
--
-- /See:/ 'deleteDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dddrqDomainName'
newtype DeleteDomain = DeleteDomain'
    { _dddrqDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDomain' smart constructor.
deleteDomain :: Text -> DeleteDomain
deleteDomain pDomainName =
    DeleteDomain'
    { _dddrqDomainName = pDomainName
    }

-- | The name of the domain you want to permanently delete.
dddrqDomainName :: Lens' DeleteDomain Text
dddrqDomainName = lens _dddrqDomainName (\ s a -> s{_dddrqDomainName = a});

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
               "DomainName" =: _dddrqDomainName]

-- | The result of a @DeleteDomain@ request. Contains the status of a newly
-- deleted domain, or no status if the domain has already been completely
-- deleted.
--
-- /See:/ 'deleteDomainResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dddrsDomainStatus'
--
-- * 'dddrsStatus'
data DeleteDomainResponse = DeleteDomainResponse'
    { _dddrsDomainStatus :: !(Maybe DomainStatus)
    , _dddrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDomainResponse' smart constructor.
deleteDomainResponse :: Int -> DeleteDomainResponse
deleteDomainResponse pStatus =
    DeleteDomainResponse'
    { _dddrsDomainStatus = Nothing
    , _dddrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
dddrsDomainStatus :: Lens' DeleteDomainResponse (Maybe DomainStatus)
dddrsDomainStatus = lens _dddrsDomainStatus (\ s a -> s{_dddrsDomainStatus = a});

-- | FIXME: Undocumented member.
dddrsStatus :: Lens' DeleteDomainResponse Int
dddrsStatus = lens _dddrsStatus (\ s a -> s{_dddrsStatus = a});
