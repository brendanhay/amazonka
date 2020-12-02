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
-- Module      : Network.AWS.CognitoIdentity.DescribeIdentityPool
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a particular identity pool, including the pool name, ID description, creation date, and current number of users.
--
--
-- You must use AWS Developer credentials to call this API.
--
module Network.AWS.CognitoIdentity.DescribeIdentityPool
    (
    -- * Creating a Request
      describeIdentityPool
    , DescribeIdentityPool
    -- * Request Lenses
    , dipIdentityPoolId

    -- * Destructuring the Response
    , identityPool
    , IdentityPool
    -- * Response Lenses
    , ipSamlProviderARNs
    , ipSupportedLoginProviders
    , ipDeveloperProviderName
    , ipOpenIdConnectProviderARNs
    , ipCognitoIdentityProviders
    , ipIdentityPoolId
    , ipIdentityPoolName
    , ipAllowUnauthenticatedIdentities
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to the DescribeIdentityPool action.
--
--
--
-- /See:/ 'describeIdentityPool' smart constructor.
newtype DescribeIdentityPool = DescribeIdentityPool'
  { _dipIdentityPoolId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIdentityPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipIdentityPoolId' - An identity pool ID in the format REGION:GUID.
describeIdentityPool
    :: Text -- ^ 'dipIdentityPoolId'
    -> DescribeIdentityPool
describeIdentityPool pIdentityPoolId_ =
  DescribeIdentityPool' {_dipIdentityPoolId = pIdentityPoolId_}


-- | An identity pool ID in the format REGION:GUID.
dipIdentityPoolId :: Lens' DescribeIdentityPool Text
dipIdentityPoolId = lens _dipIdentityPoolId (\ s a -> s{_dipIdentityPoolId = a})

instance AWSRequest DescribeIdentityPool where
        type Rs DescribeIdentityPool = IdentityPool
        request = postJSON cognitoIdentity
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DescribeIdentityPool where

instance NFData DescribeIdentityPool where

instance ToHeaders DescribeIdentityPool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.DescribeIdentityPool" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeIdentityPool where
        toJSON DescribeIdentityPool'{..}
          = object
              (catMaybes
                 [Just ("IdentityPoolId" .= _dipIdentityPoolId)])

instance ToPath DescribeIdentityPool where
        toPath = const "/"

instance ToQuery DescribeIdentityPool where
        toQuery = const mempty
