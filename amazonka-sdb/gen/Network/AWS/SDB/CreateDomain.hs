{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.CreateDomain
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The @CreateDomain@ operation creates a new domain. The domain name
-- should be unique among the domains associated with the Access Key ID
-- provided in the request. The @CreateDomain@ operation may take 10 or
-- more seconds to complete.
--
-- The client can create up to 100 domains per account.
--
-- If the client requires additional domains, go to
-- <http://aws.amazon.com/contact-us/simpledb-limit-request/>.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_CreateDomain.html AWS API Reference> for CreateDomain.
module Network.AWS.SDB.CreateDomain
    (
    -- * Creating a Request
      CreateDomain
    , createDomain
    -- * Request Lenses
    , cdDomainName

    -- * Destructuring the Response
    , CreateDomainResponse
    , createDomainResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SDB.Types

-- | /See:/ 'createDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdDomainName'
newtype CreateDomain = CreateDomain'
    { _cdDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDomain' smart constructor.
createDomain :: Text -> CreateDomain
createDomain pDomainName_ =
    CreateDomain'
    { _cdDomainName = pDomainName_
    }

-- | The name of the domain to create. The name can range between 3 and 255
-- characters and can contain the following characters: a-z, A-Z, 0-9,
-- \'_\', \'-\', and \'.\'.
cdDomainName :: Lens' CreateDomain Text
cdDomainName = lens _cdDomainName (\ s a -> s{_cdDomainName = a});

instance AWSRequest CreateDomain where
        type Sv CreateDomain = SDB
        type Rs CreateDomain = CreateDomainResponse
        request = postQuery
        response = receiveNull CreateDomainResponse'

instance ToHeaders CreateDomain where
        toHeaders = const mempty

instance ToPath CreateDomain where
        toPath = const "/"

instance ToQuery CreateDomain where
        toQuery CreateDomain'{..}
          = mconcat
              ["Action" =: ("CreateDomain" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               "DomainName" =: _cdDomainName]

-- | /See:/ 'createDomainResponse' smart constructor.
data CreateDomainResponse =
    CreateDomainResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDomainResponse' smart constructor.
createDomainResponse :: CreateDomainResponse
createDomainResponse = CreateDomainResponse'
