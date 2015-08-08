{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.CreateDomain
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new search domain. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/creating-domains.html Creating a Search Domain>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_CreateDomain.html AWS API Reference> for CreateDomain.
module Network.AWS.CloudSearch.CreateDomain
    (
    -- * Creating a Request
      CreateDomain
    , createDomain
    -- * Request Lenses
    , cdDomainName

    -- * Destructuring the Response
    , CreateDomainResponse
    , createDomainResponse
    -- * Response Lenses
    , cdrsDomainStatus
    , cdrsStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @CreateDomain@ operation. Specifies
-- a name for the new search domain.
--
-- /See:/ 'createDomain' smart constructor.
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

-- | A name for the domain you are creating. Allowed characters are a-z
-- (lower-case letters), 0-9, and hyphen (-). Domain names must start with
-- a letter or number and be at least 3 and no more than 28 characters
-- long.
cdDomainName :: Lens' CreateDomain Text
cdDomainName = lens _cdDomainName (\ s a -> s{_cdDomainName = a});

instance AWSRequest CreateDomain where
        type Sv CreateDomain = CloudSearch
        type Rs CreateDomain = CreateDomainResponse
        request = postQuery
        response
          = receiveXMLWrapper "CreateDomainResult"
              (\ s h x ->
                 CreateDomainResponse' <$>
                   (x .@? "DomainStatus") <*> (pure (fromEnum s)))

instance ToHeaders CreateDomain where
        toHeaders = const mempty

instance ToPath CreateDomain where
        toPath = const "/"

instance ToQuery CreateDomain where
        toQuery CreateDomain'{..}
          = mconcat
              ["Action" =: ("CreateDomain" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _cdDomainName]

-- | The result of a @CreateDomainRequest@. Contains the status of a newly
-- created domain.
--
-- /See:/ 'createDomainResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrsDomainStatus'
--
-- * 'cdrsStatus'
data CreateDomainResponse = CreateDomainResponse'
    { _cdrsDomainStatus :: !(Maybe DomainStatus)
    , _cdrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDomainResponse' smart constructor.
createDomainResponse :: Int -> CreateDomainResponse
createDomainResponse pStatus_ =
    CreateDomainResponse'
    { _cdrsDomainStatus = Nothing
    , _cdrsStatus = pStatus_
    }

-- | Undocumented member.
cdrsDomainStatus :: Lens' CreateDomainResponse (Maybe DomainStatus)
cdrsDomainStatus = lens _cdrsDomainStatus (\ s a -> s{_cdrsDomainStatus = a});

-- | Undocumented member.
cdrsStatus :: Lens' CreateDomainResponse Int
cdrsStatus = lens _cdrsStatus (\ s a -> s{_cdrsStatus = a});
