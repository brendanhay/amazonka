{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_CreateDomain.html>
module Network.AWS.CloudSearch.CreateDomain
    (
    -- * Request
      CreateDomain
    -- ** Request constructor
    , createDomain
    -- ** Request lenses
    , cdDomainName

    -- * Response
    , CreateDomainResponse
    -- ** Response constructor
    , createDomainResponse
    -- ** Response lenses
    , cdrDomainStatus
    , cdrStatus
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
createDomain pDomainName =
    CreateDomain'
    { _cdDomainName = pDomainName
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
        request = post
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
-- * 'cdrDomainStatus'
--
-- * 'cdrStatus'
data CreateDomainResponse = CreateDomainResponse'
    { _cdrDomainStatus :: !(Maybe DomainStatus)
    , _cdrStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDomainResponse' smart constructor.
createDomainResponse :: Int -> CreateDomainResponse
createDomainResponse pStatus =
    CreateDomainResponse'
    { _cdrDomainStatus = Nothing
    , _cdrStatus = pStatus
    }

-- | FIXME: Undocumented member.
cdrDomainStatus :: Lens' CreateDomainResponse (Maybe DomainStatus)
cdrDomainStatus = lens _cdrDomainStatus (\ s a -> s{_cdrDomainStatus = a});

-- | FIXME: Undocumented member.
cdrStatus :: Lens' CreateDomainResponse Int
cdrStatus = lens _cdrStatus (\ s a -> s{_cdrStatus = a});
