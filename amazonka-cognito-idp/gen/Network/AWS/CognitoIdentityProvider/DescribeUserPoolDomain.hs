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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a domain.
--
--
module Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain
    (
    -- * Creating a Request
      describeUserPoolDomain
    , DescribeUserPoolDomain
    -- * Request Lenses
    , dDomain

    -- * Destructuring the Response
    , describeUserPoolDomainResponse
    , DescribeUserPoolDomainResponse
    -- * Response Lenses
    , drsDomainDescription
    , drsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUserPoolDomain' smart constructor.
newtype DescribeUserPoolDomain = DescribeUserPoolDomain'
  { _dDomain :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserPoolDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDomain' - The domain string.
describeUserPoolDomain
    :: Text -- ^ 'dDomain'
    -> DescribeUserPoolDomain
describeUserPoolDomain pDomain_ = DescribeUserPoolDomain' {_dDomain = pDomain_}


-- | The domain string.
dDomain :: Lens' DescribeUserPoolDomain Text
dDomain = lens _dDomain (\ s a -> s{_dDomain = a})

instance AWSRequest DescribeUserPoolDomain where
        type Rs DescribeUserPoolDomain =
             DescribeUserPoolDomainResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserPoolDomainResponse' <$>
                   (x .?> "DomainDescription") <*> (pure (fromEnum s)))

instance Hashable DescribeUserPoolDomain where

instance NFData DescribeUserPoolDomain where

instance ToHeaders DescribeUserPoolDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DescribeUserPoolDomain"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeUserPoolDomain where
        toJSON DescribeUserPoolDomain'{..}
          = object (catMaybes [Just ("Domain" .= _dDomain)])

instance ToPath DescribeUserPoolDomain where
        toPath = const "/"

instance ToQuery DescribeUserPoolDomain where
        toQuery = const mempty

-- | /See:/ 'describeUserPoolDomainResponse' smart constructor.
data DescribeUserPoolDomainResponse = DescribeUserPoolDomainResponse'
  { _drsDomainDescription :: !(Maybe DomainDescriptionType)
  , _drsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserPoolDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsDomainDescription' - A domain description object containing information about the domain.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeUserPoolDomainResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeUserPoolDomainResponse
describeUserPoolDomainResponse pResponseStatus_ =
  DescribeUserPoolDomainResponse'
    {_drsDomainDescription = Nothing, _drsResponseStatus = pResponseStatus_}


-- | A domain description object containing information about the domain.
drsDomainDescription :: Lens' DescribeUserPoolDomainResponse (Maybe DomainDescriptionType)
drsDomainDescription = lens _drsDomainDescription (\ s a -> s{_drsDomainDescription = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeUserPoolDomainResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeUserPoolDomainResponse where
