{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.DeleteDomain
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteDomain@ operation deletes a domain. Any items (and their
-- attributes) in the domain are deleted as well. The @DeleteDomain@
-- operation might take 10 or more seconds to complete.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_DeleteDomain.html AWS API Reference> for DeleteDomain.
module Network.AWS.SDB.DeleteDomain
    (
    -- * Creating a Request
      DeleteDomain
    , deleteDomain
    -- * Request Lenses
    , ddDomainName

    -- * Destructuring the Response
    , DeleteDomainResponse
    , deleteDomainResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SDB.Types
import           Network.AWS.SDB.Types.Product

-- | /See:/ 'deleteDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddDomainName'
newtype DeleteDomain = DeleteDomain'
    { _ddDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDomain' smart constructor.
deleteDomain :: Text -> DeleteDomain
deleteDomain pDomainName_ =
    DeleteDomain'
    { _ddDomainName = pDomainName_
    }

-- | The name of the domain to delete.
ddDomainName :: Lens' DeleteDomain Text
ddDomainName = lens _ddDomainName (\ s a -> s{_ddDomainName = a});

instance AWSRequest DeleteDomain where
        type Sv DeleteDomain = SDB
        type Rs DeleteDomain = DeleteDomainResponse
        request = postQuery
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
data DeleteDomainResponse =
    DeleteDomainResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDomainResponse' smart constructor.
deleteDomainResponse :: DeleteDomainResponse
deleteDomainResponse = DeleteDomainResponse'
