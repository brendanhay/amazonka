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
-- Module      : Network.AWS.CertificateManagerPCA.ListCertificateAuthorities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the private certificate authorities that you created by using the 'CreateCertificateAuthority' function.
--
--
module Network.AWS.CertificateManagerPCA.ListCertificateAuthorities
    (
    -- * Creating a Request
      listCertificateAuthorities
    , ListCertificateAuthorities
    -- * Request Lenses
    , lcaNextToken
    , lcaMaxResults

    -- * Destructuring the Response
    , listCertificateAuthoritiesResponse
    , ListCertificateAuthoritiesResponse
    -- * Response Lenses
    , lcarsCertificateAuthorities
    , lcarsNextToken
    , lcarsResponseStatus
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCertificateAuthorities' smart constructor.
data ListCertificateAuthorities = ListCertificateAuthorities'
  { _lcaNextToken  :: !(Maybe Text)
  , _lcaMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCertificateAuthorities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcaNextToken' - Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of the @NextToken@ parameter from the response you just received.
--
-- * 'lcaMaxResults' - Use this parameter when paginating results to specify the maximum number of items to return in the response on each page. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
listCertificateAuthorities
    :: ListCertificateAuthorities
listCertificateAuthorities =
  ListCertificateAuthorities'
    {_lcaNextToken = Nothing, _lcaMaxResults = Nothing}


-- | Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of the @NextToken@ parameter from the response you just received.
lcaNextToken :: Lens' ListCertificateAuthorities (Maybe Text)
lcaNextToken = lens _lcaNextToken (\ s a -> s{_lcaNextToken = a})

-- | Use this parameter when paginating results to specify the maximum number of items to return in the response on each page. If additional items exist beyond the number you specify, the @NextToken@ element is sent in the response. Use this @NextToken@ value in a subsequent request to retrieve additional items.
lcaMaxResults :: Lens' ListCertificateAuthorities (Maybe Natural)
lcaMaxResults = lens _lcaMaxResults (\ s a -> s{_lcaMaxResults = a}) . mapping _Nat

instance AWSRequest ListCertificateAuthorities where
        type Rs ListCertificateAuthorities =
             ListCertificateAuthoritiesResponse
        request = postJSON certificateManagerPCA
        response
          = receiveJSON
              (\ s h x ->
                 ListCertificateAuthoritiesResponse' <$>
                   (x .?> "CertificateAuthorities" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListCertificateAuthorities where

instance NFData ListCertificateAuthorities where

instance ToHeaders ListCertificateAuthorities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.ListCertificateAuthorities" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCertificateAuthorities where
        toJSON ListCertificateAuthorities'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lcaNextToken,
                  ("MaxResults" .=) <$> _lcaMaxResults])

instance ToPath ListCertificateAuthorities where
        toPath = const "/"

instance ToQuery ListCertificateAuthorities where
        toQuery = const mempty

-- | /See:/ 'listCertificateAuthoritiesResponse' smart constructor.
data ListCertificateAuthoritiesResponse = ListCertificateAuthoritiesResponse'
  { _lcarsCertificateAuthorities :: !(Maybe [CertificateAuthority])
  , _lcarsNextToken              :: !(Maybe Text)
  , _lcarsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCertificateAuthoritiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcarsCertificateAuthorities' - Summary information about each certificate authority you have created.
--
-- * 'lcarsNextToken' - When the list is truncated, this value is present and should be used for the @NextToken@ parameter in a subsequent pagination request.
--
-- * 'lcarsResponseStatus' - -- | The response status code.
listCertificateAuthoritiesResponse
    :: Int -- ^ 'lcarsResponseStatus'
    -> ListCertificateAuthoritiesResponse
listCertificateAuthoritiesResponse pResponseStatus_ =
  ListCertificateAuthoritiesResponse'
    { _lcarsCertificateAuthorities = Nothing
    , _lcarsNextToken = Nothing
    , _lcarsResponseStatus = pResponseStatus_
    }


-- | Summary information about each certificate authority you have created.
lcarsCertificateAuthorities :: Lens' ListCertificateAuthoritiesResponse [CertificateAuthority]
lcarsCertificateAuthorities = lens _lcarsCertificateAuthorities (\ s a -> s{_lcarsCertificateAuthorities = a}) . _Default . _Coerce

-- | When the list is truncated, this value is present and should be used for the @NextToken@ parameter in a subsequent pagination request.
lcarsNextToken :: Lens' ListCertificateAuthoritiesResponse (Maybe Text)
lcarsNextToken = lens _lcarsNextToken (\ s a -> s{_lcarsNextToken = a})

-- | -- | The response status code.
lcarsResponseStatus :: Lens' ListCertificateAuthoritiesResponse Int
lcarsResponseStatus = lens _lcarsResponseStatus (\ s a -> s{_lcarsResponseStatus = a})

instance NFData ListCertificateAuthoritiesResponse
         where
