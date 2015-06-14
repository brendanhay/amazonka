{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoIdentity.DescribeIdentity
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

-- | Returns metadata related to the given identity, including when the
-- identity was created and any associated linked logins.
--
-- You must use AWS Developer credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_DescribeIdentity.html>
module Network.AWS.CognitoIdentity.DescribeIdentity
    (
    -- * Request
      DescribeIdentity
    -- ** Request constructor
    , describeIdentity
    -- ** Request lenses
    , diIdentityId

    -- * Response
    , IdentityDescription
    -- ** Response constructor
    , identityDescription
    -- ** Response lenses
    , idLastModifiedDate
    , idCreationDate
    , idLogins
    , idIdentityId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoIdentity.Types

-- | /See:/ 'describeIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diIdentityId'
newtype DescribeIdentity = DescribeIdentity'{_diIdentityId :: Text} deriving (Eq, Read, Show)

-- | 'DescribeIdentity' smart constructor.
describeIdentity :: Text -> DescribeIdentity
describeIdentity pIdentityId = DescribeIdentity'{_diIdentityId = pIdentityId};

-- | A unique identifier in the format REGION:GUID.
diIdentityId :: Lens' DescribeIdentity Text
diIdentityId = lens _diIdentityId (\ s a -> s{_diIdentityId = a});

instance AWSRequest DescribeIdentity where
        type Sv DescribeIdentity = CognitoIdentity
        type Rs DescribeIdentity = IdentityDescription
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

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
          = object ["IdentityId" .= _diIdentityId]

instance ToPath DescribeIdentity where
        toPath = const "/"

instance ToQuery DescribeIdentity where
        toQuery = const mempty
