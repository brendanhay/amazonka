{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53Domains.RetrieveDomainAuthCode
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

-- | This operation returns the AuthCode for the domain. To transfer a domain
-- to another registrar, you provide this value to the new registrar.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-RetrieveDomainAuthCode.html>
module Network.AWS.Route53Domains.RetrieveDomainAuthCode
    (
    -- * Request
      RetrieveDomainAuthCode
    -- ** Request constructor
    , retrieveDomainAuthCode
    -- ** Request lenses
    , rdacDomainName

    -- * Response
    , RetrieveDomainAuthCodeResponse
    -- ** Response constructor
    , retrieveDomainAuthCodeResponse
    -- ** Response lenses
    , rdacrAuthCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types

-- | /See:/ 'retrieveDomainAuthCode' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdacDomainName'
newtype RetrieveDomainAuthCode = RetrieveDomainAuthCode'{_rdacDomainName :: Text} deriving (Eq, Read, Show)

-- | 'RetrieveDomainAuthCode' smart constructor.
retrieveDomainAuthCode :: Text -> RetrieveDomainAuthCode
retrieveDomainAuthCode pDomainName = RetrieveDomainAuthCode'{_rdacDomainName = pDomainName};

-- | The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z,
-- the numbers 0 through 9, and hyphen (-). Internationalized Domain Names
-- are not supported.
--
-- Required: Yes
rdacDomainName :: Lens' RetrieveDomainAuthCode Text
rdacDomainName = lens _rdacDomainName (\ s a -> s{_rdacDomainName = a});

instance AWSRequest RetrieveDomainAuthCode where
        type Sv RetrieveDomainAuthCode = Route53Domains
        type Rs RetrieveDomainAuthCode =
             RetrieveDomainAuthCodeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RetrieveDomainAuthCodeResponse' <$>
                   (x .:> "AuthCode"))

instance ToHeaders RetrieveDomainAuthCode where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.RetrieveDomainAuthCode" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RetrieveDomainAuthCode where
        toJSON RetrieveDomainAuthCode'{..}
          = object ["DomainName" .= _rdacDomainName]

instance ToPath RetrieveDomainAuthCode where
        toPath = const "/"

instance ToQuery RetrieveDomainAuthCode where
        toQuery = const mempty

-- | /See:/ 'retrieveDomainAuthCodeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdacrAuthCode'
newtype RetrieveDomainAuthCodeResponse = RetrieveDomainAuthCodeResponse'{_rdacrAuthCode :: Sensitive Text} deriving (Eq, Read, Show)

-- | 'RetrieveDomainAuthCodeResponse' smart constructor.
retrieveDomainAuthCodeResponse :: Text -> RetrieveDomainAuthCodeResponse
retrieveDomainAuthCodeResponse pAuthCode = RetrieveDomainAuthCodeResponse'{_rdacrAuthCode = _Sensitive # pAuthCode};

-- | The authorization code for the domain.
--
-- Type: String
rdacrAuthCode :: Lens' RetrieveDomainAuthCodeResponse Text
rdacrAuthCode = lens _rdacrAuthCode (\ s a -> s{_rdacrAuthCode = a}) . _Sensitive;
