{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.DescribeIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata related to the given identity, including when the
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
    , dirqIdentityId

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

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the @DescribeIdentity@ action.
--
-- /See:/ 'describeIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirqIdentityId'
newtype DescribeIdentity = DescribeIdentity'
    { _dirqIdentityId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeIdentity' smart constructor.
describeIdentity :: Text -> DescribeIdentity
describeIdentity pIdentityId_ =
    DescribeIdentity'
    { _dirqIdentityId = pIdentityId_
    }

-- | A unique identifier in the format REGION:GUID.
dirqIdentityId :: Lens' DescribeIdentity Text
dirqIdentityId = lens _dirqIdentityId (\ s a -> s{_dirqIdentityId = a});

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
          = object ["IdentityId" .= _dirqIdentityId]

instance ToPath DescribeIdentity where
        toPath = const "/"

instance ToQuery DescribeIdentity where
        toQuery = const mempty
