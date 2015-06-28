{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.CreateDomain
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

-- | Creates a new search domain. For more information, see
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
    } deriving (Eq,Read,Show)

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
                   (x .@? "DomainStatus") <*> (pure s))

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
    , _cdrStatus       :: !Status
    } deriving (Eq,Read,Show)

-- | 'CreateDomainResponse' smart constructor.
createDomainResponse :: Status -> CreateDomainResponse
createDomainResponse pStatus =
    CreateDomainResponse'
    { _cdrDomainStatus = Nothing
    , _cdrStatus = pStatus
    }

-- | FIXME: Undocumented member.
cdrDomainStatus :: Lens' CreateDomainResponse (Maybe DomainStatus)
cdrDomainStatus = lens _cdrDomainStatus (\ s a -> s{_cdrDomainStatus = a});

-- | FIXME: Undocumented member.
cdrStatus :: Lens' CreateDomainResponse Status
cdrStatus = lens _cdrStatus (\ s a -> s{_cdrStatus = a});
