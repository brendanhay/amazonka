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
-- Module      : Network.AWS.CognitoIdentity.DescribeIdentity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata related to the given identity, including when the identity was created and any associated linked logins.
--
--
-- You must use AWS Developer credentials to call this API.
--
module Network.AWS.CognitoIdentity.DescribeIdentity
    (
    -- * Creating a Request
      describeIdentity
    , DescribeIdentity
    -- * Request Lenses
    , diIdentityId

    -- * Destructuring the Response
    , identityDescription
    , IdentityDescription
    -- * Response Lenses
    , idLastModifiedDate
    , idCreationDate
    , idLogins
    , idIdentityId
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to the @DescribeIdentity@ action.
--
--
--
-- /See:/ 'describeIdentity' smart constructor.
newtype DescribeIdentity = DescribeIdentity'
  { _diIdentityId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diIdentityId' - A unique identifier in the format REGION:GUID.
describeIdentity
    :: Text -- ^ 'diIdentityId'
    -> DescribeIdentity
describeIdentity pIdentityId_ = DescribeIdentity' {_diIdentityId = pIdentityId_}


-- | A unique identifier in the format REGION:GUID.
diIdentityId :: Lens' DescribeIdentity Text
diIdentityId = lens _diIdentityId (\ s a -> s{_diIdentityId = a})

instance AWSRequest DescribeIdentity where
        type Rs DescribeIdentity = IdentityDescription
        request = postJSON cognitoIdentity
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DescribeIdentity where

instance NFData DescribeIdentity where

instance ToHeaders DescribeIdentity where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.DescribeIdentity" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeIdentity where
        toJSON DescribeIdentity'{..}
          = object
              (catMaybes [Just ("IdentityId" .= _diIdentityId)])

instance ToPath DescribeIdentity where
        toPath = const "/"

instance ToQuery DescribeIdentity where
        toQuery = const mempty
