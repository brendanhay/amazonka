{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SDB.DeleteDomain
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

-- | The @DeleteDomain@ operation deletes a domain. Any items (and their
-- attributes) in the domain are deleted as well. The @DeleteDomain@
-- operation might take 10 or more seconds to complete.
--
-- <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_DeleteDomain.html>
module Network.AWS.SDB.DeleteDomain
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
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SDB.Types

-- | /See:/ 'deleteDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddDomainName'
newtype DeleteDomain = DeleteDomain'{_ddDomainName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteDomain' smart constructor.
deleteDomain :: Text -> DeleteDomain
deleteDomain pDomainName = DeleteDomain'{_ddDomainName = pDomainName};

-- | The name of the domain to delete.
ddDomainName :: Lens' DeleteDomain Text
ddDomainName = lens _ddDomainName (\ s a -> s{_ddDomainName = a});

instance AWSRequest DeleteDomain where
        type Sv DeleteDomain = SDB
        type Rs DeleteDomain = DeleteDomainResponse
        request = post
        response = receiveNull DeleteDomainResponse'

instance ToHeaders DeleteDomain where
        toHeaders = const mempty

instance ToPath DeleteDomain where
        toPath = const "/"

instance ToQuery DeleteDomain where
        toQuery DeleteDomain'{..}
          = mconcat
              ["Action" =: ("DeleteDomain" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               "DomainName" =: _ddDomainName]

-- | /See:/ 'deleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse' deriving (Eq, Read, Show)

-- | 'DeleteDomainResponse' smart constructor.
deleteDomainResponse :: DeleteDomainResponse
deleteDomainResponse = DeleteDomainResponse';
