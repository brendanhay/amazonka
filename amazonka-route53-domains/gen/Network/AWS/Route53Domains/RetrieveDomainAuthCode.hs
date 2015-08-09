{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.RetrieveDomainAuthCode
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the AuthCode for the domain. To transfer a domain
-- to another registrar, you provide this value to the new registrar.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/api-RetrieveDomainAuthCode.html AWS API Reference> for RetrieveDomainAuthCode.
module Network.AWS.Route53Domains.RetrieveDomainAuthCode
    (
    -- * Creating a Request
      RetrieveDomainAuthCode
    , retrieveDomainAuthCode
    -- * Request Lenses
    , rdacDomainName

    -- * Destructuring the Response
    , RetrieveDomainAuthCodeResponse
    , retrieveDomainAuthCodeResponse
    -- * Response Lenses
    , rdacrsStatus
    , rdacrsAuthCode
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | The RetrieveDomainAuthCode request includes the following element.
--
-- /See:/ 'retrieveDomainAuthCode' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdacDomainName'
newtype RetrieveDomainAuthCode = RetrieveDomainAuthCode'
    { _rdacDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RetrieveDomainAuthCode' smart constructor.
retrieveDomainAuthCode :: Text -> RetrieveDomainAuthCode
retrieveDomainAuthCode pDomainName_ =
    RetrieveDomainAuthCode'
    { _rdacDomainName = pDomainName_
    }

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
                   (pure (fromEnum s)) <*> (x .:> "AuthCode"))

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

-- | The RetrieveDomainAuthCode response includes the following element.
--
-- /See:/ 'retrieveDomainAuthCodeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdacrsStatus'
--
-- * 'rdacrsAuthCode'
data RetrieveDomainAuthCodeResponse = RetrieveDomainAuthCodeResponse'
    { _rdacrsStatus   :: !Int
    , _rdacrsAuthCode :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RetrieveDomainAuthCodeResponse' smart constructor.
retrieveDomainAuthCodeResponse :: Int -> Text -> RetrieveDomainAuthCodeResponse
retrieveDomainAuthCodeResponse pStatus_ pAuthCode_ =
    RetrieveDomainAuthCodeResponse'
    { _rdacrsStatus = pStatus_
    , _rdacrsAuthCode = _Sensitive # pAuthCode_
    }

-- | Undocumented member.
rdacrsStatus :: Lens' RetrieveDomainAuthCodeResponse Int
rdacrsStatus = lens _rdacrsStatus (\ s a -> s{_rdacrsStatus = a});

-- | The authorization code for the domain.
--
-- Type: String
rdacrsAuthCode :: Lens' RetrieveDomainAuthCodeResponse Text
rdacrsAuthCode = lens _rdacrsAuthCode (\ s a -> s{_rdacrsAuthCode = a}) . _Sensitive;
