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
-- Module      : Network.AWS.SES.VerifyDomainDkim
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of DKIM tokens for a domain. DKIM /tokens/ are character strings that represent your domain's identity. Using these tokens, you will need to create DNS CNAME records that point to DKIM public keys hosted by Amazon SES. Amazon Web Services will eventually detect that you have updated your DNS records; this detection process may take up to 72 hours. Upon successful detection, Amazon SES will be able to DKIM-sign email originating from that domain.
--
--
-- You can execute this operation no more than once per second.
--
-- To enable or disable Easy DKIM signing for a domain, use the @SetIdentityDkimEnabled@ operation.
--
-- For more information about creating DNS records using DKIM tokens, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide> .
--
module Network.AWS.SES.VerifyDomainDkim
    (
    -- * Creating a Request
      verifyDomainDkim
    , VerifyDomainDkim
    -- * Request Lenses
    , vddDomain

    -- * Destructuring the Response
    , verifyDomainDkimResponse
    , VerifyDomainDkimResponse
    -- * Response Lenses
    , vddrsResponseStatus
    , vddrsDkimTokens
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to generate the CNAME records needed to set up Easy DKIM with Amazon SES. For more information about setting up Easy DKIM, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'verifyDomainDkim' smart constructor.
newtype VerifyDomainDkim = VerifyDomainDkim'
  { _vddDomain :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyDomainDkim' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vddDomain' - The name of the domain to be verified for Easy DKIM signing.
verifyDomainDkim
    :: Text -- ^ 'vddDomain'
    -> VerifyDomainDkim
verifyDomainDkim pDomain_ = VerifyDomainDkim' {_vddDomain = pDomain_}


-- | The name of the domain to be verified for Easy DKIM signing.
vddDomain :: Lens' VerifyDomainDkim Text
vddDomain = lens _vddDomain (\ s a -> s{_vddDomain = a})

instance AWSRequest VerifyDomainDkim where
        type Rs VerifyDomainDkim = VerifyDomainDkimResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "VerifyDomainDkimResult"
              (\ s h x ->
                 VerifyDomainDkimResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "DkimTokens" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable VerifyDomainDkim where

instance NFData VerifyDomainDkim where

instance ToHeaders VerifyDomainDkim where
        toHeaders = const mempty

instance ToPath VerifyDomainDkim where
        toPath = const "/"

instance ToQuery VerifyDomainDkim where
        toQuery VerifyDomainDkim'{..}
          = mconcat
              ["Action" =: ("VerifyDomainDkim" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Domain" =: _vddDomain]

-- | Returns CNAME records that you must publish to the DNS server of your domain to set up Easy DKIM with Amazon SES.
--
--
--
-- /See:/ 'verifyDomainDkimResponse' smart constructor.
data VerifyDomainDkimResponse = VerifyDomainDkimResponse'
  { _vddrsResponseStatus :: !Int
  , _vddrsDkimTokens     :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifyDomainDkimResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vddrsResponseStatus' - -- | The response status code.
--
-- * 'vddrsDkimTokens' - A set of character strings that represent the domain's identity. If the identity is an email address, the tokens represent the domain of that address. Using these tokens, you will need to create DNS CNAME records that point to DKIM public keys hosted by Amazon SES. Amazon Web Services will eventually detect that you have updated your DNS records; this detection process may take up to 72 hours. Upon successful detection, Amazon SES will be able to DKIM-sign emails originating from that domain. For more information about creating DNS records using DKIM tokens, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide> .
verifyDomainDkimResponse
    :: Int -- ^ 'vddrsResponseStatus'
    -> VerifyDomainDkimResponse
verifyDomainDkimResponse pResponseStatus_ =
  VerifyDomainDkimResponse'
    {_vddrsResponseStatus = pResponseStatus_, _vddrsDkimTokens = mempty}


-- | -- | The response status code.
vddrsResponseStatus :: Lens' VerifyDomainDkimResponse Int
vddrsResponseStatus = lens _vddrsResponseStatus (\ s a -> s{_vddrsResponseStatus = a})

-- | A set of character strings that represent the domain's identity. If the identity is an email address, the tokens represent the domain of that address. Using these tokens, you will need to create DNS CNAME records that point to DKIM public keys hosted by Amazon SES. Amazon Web Services will eventually detect that you have updated your DNS records; this detection process may take up to 72 hours. Upon successful detection, Amazon SES will be able to DKIM-sign emails originating from that domain. For more information about creating DNS records using DKIM tokens, go to the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide> .
vddrsDkimTokens :: Lens' VerifyDomainDkimResponse [Text]
vddrsDkimTokens = lens _vddrsDkimTokens (\ s a -> s{_vddrsDkimTokens = a}) . _Coerce

instance NFData VerifyDomainDkimResponse where
