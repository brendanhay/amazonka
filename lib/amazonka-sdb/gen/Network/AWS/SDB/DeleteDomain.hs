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
-- Module      : Network.AWS.SDB.DeleteDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteDomain@ operation deletes a domain. Any items (and their attributes) in the domain are deleted as well. The @DeleteDomain@ operation might take 10 or more seconds to complete.
--
--
module Network.AWS.SDB.DeleteDomain
    (
    -- * Creating a Request
      deleteDomain
    , DeleteDomain
    -- * Request Lenses
    , ddDomainName

    -- * Destructuring the Response
    , deleteDomainResponse
    , DeleteDomainResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SDB.Types
import Network.AWS.SDB.Types.Product

-- | /See:/ 'deleteDomain' smart constructor.
newtype DeleteDomain = DeleteDomain'
  { _ddDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDomainName' - The name of the domain to delete.
deleteDomain
    :: Text -- ^ 'ddDomainName'
    -> DeleteDomain
deleteDomain pDomainName_ = DeleteDomain' {_ddDomainName = pDomainName_}


-- | The name of the domain to delete.
ddDomainName :: Lens' DeleteDomain Text
ddDomainName = lens _ddDomainName (\ s a -> s{_ddDomainName = a})

instance AWSRequest DeleteDomain where
        type Rs DeleteDomain = DeleteDomainResponse
        request = postQuery sdb
        response = receiveNull DeleteDomainResponse'

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
               "Version" =: ("2009-04-15" :: ByteString),
               "DomainName" =: _ddDomainName]

-- | /See:/ 'deleteDomainResponse' smart constructor.
data DeleteDomainResponse =
  DeleteDomainResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDomainResponse' with the minimum fields required to make a request.
--
deleteDomainResponse
    :: DeleteDomainResponse
deleteDomainResponse = DeleteDomainResponse'


instance NFData DeleteDomainResponse where
