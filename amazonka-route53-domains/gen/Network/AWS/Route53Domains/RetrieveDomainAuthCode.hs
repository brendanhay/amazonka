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
-- Module      : Network.AWS.Route53Domains.RetrieveDomainAuthCode
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the AuthCode for the domain. To transfer a domain
-- to another registrar, you provide this value to the new registrar.
module Network.AWS.Route53Domains.RetrieveDomainAuthCode
    (
    -- * Creating a Request
      retrieveDomainAuthCode
    , RetrieveDomainAuthCode
    -- * Request Lenses
    , rdacDomainName

    -- * Destructuring the Response
    , retrieveDomainAuthCodeResponse
    , RetrieveDomainAuthCodeResponse
    -- * Response Lenses
    , rdacrsResponseStatus
    , rdacrsAuthCode
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | The RetrieveDomainAuthCode request includes the following element.
--
-- /See:/ 'retrieveDomainAuthCode' smart constructor.
newtype RetrieveDomainAuthCode = RetrieveDomainAuthCode'
    { _rdacDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RetrieveDomainAuthCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdacDomainName'
retrieveDomainAuthCode
    :: Text -- ^ 'rdacDomainName'
    -> RetrieveDomainAuthCode
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
        type Rs RetrieveDomainAuthCode =
             RetrieveDomainAuthCodeResponse
        request = postJSON route53Domains
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
          = object
              (catMaybes [Just ("DomainName" .= _rdacDomainName)])

instance ToPath RetrieveDomainAuthCode where
        toPath = const "/"

instance ToQuery RetrieveDomainAuthCode where
        toQuery = const mempty

-- | The RetrieveDomainAuthCode response includes the following element.
--
-- /See:/ 'retrieveDomainAuthCodeResponse' smart constructor.
data RetrieveDomainAuthCodeResponse = RetrieveDomainAuthCodeResponse'
    { _rdacrsResponseStatus :: !Int
    , _rdacrsAuthCode       :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RetrieveDomainAuthCodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdacrsResponseStatus'
--
-- * 'rdacrsAuthCode'
retrieveDomainAuthCodeResponse
    :: Int -- ^ 'rdacrsResponseStatus'
    -> Text -- ^ 'rdacrsAuthCode'
    -> RetrieveDomainAuthCodeResponse
retrieveDomainAuthCodeResponse pResponseStatus_ pAuthCode_ =
    RetrieveDomainAuthCodeResponse'
    { _rdacrsResponseStatus = pResponseStatus_
    , _rdacrsAuthCode = _Sensitive # pAuthCode_
    }

-- | The response status code.
rdacrsResponseStatus :: Lens' RetrieveDomainAuthCodeResponse Int
rdacrsResponseStatus = lens _rdacrsResponseStatus (\ s a -> s{_rdacrsResponseStatus = a});

-- | The authorization code for the domain.
--
-- Type: String
rdacrsAuthCode :: Lens' RetrieveDomainAuthCodeResponse Text
rdacrsAuthCode = lens _rdacrsAuthCode (\ s a -> s{_rdacrsAuthCode = a}) . _Sensitive;
