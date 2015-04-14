{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.DescribeIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns metadata related to the given identity, including when the identity
-- was created and any associated linked logins.
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
    , DescribeIdentityResponse
    -- ** Response constructor
    , describeIdentityResponse
    -- ** Response lenses
    , dirCreationDate
    , dirIdentityId
    , dirLastModifiedDate
    , dirLogins
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

newtype DescribeIdentity = DescribeIdentity
    { _diIdentityId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DescribeIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diIdentityId' @::@ 'Text'
--
describeIdentity :: Text -- ^ 'diIdentityId'
                 -> DescribeIdentity
describeIdentity p1 = DescribeIdentity
    { _diIdentityId = p1
    }

-- | A unique identifier in the format REGION:GUID.
diIdentityId :: Lens' DescribeIdentity Text
diIdentityId = lens _diIdentityId (\s a -> s { _diIdentityId = a })

data DescribeIdentityResponse = DescribeIdentityResponse
    { _dirCreationDate     :: Maybe POSIX
    , _dirIdentityId       :: Maybe Text
    , _dirLastModifiedDate :: Maybe POSIX
    , _dirLogins           :: List "Logins" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeIdentityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirCreationDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'dirIdentityId' @::@ 'Maybe' 'Text'
--
-- * 'dirLastModifiedDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'dirLogins' @::@ ['Text']
--
describeIdentityResponse :: DescribeIdentityResponse
describeIdentityResponse = DescribeIdentityResponse
    { _dirIdentityId       = Nothing
    , _dirLogins           = mempty
    , _dirCreationDate     = Nothing
    , _dirLastModifiedDate = Nothing
    }

-- | Date on which the identity was created.
dirCreationDate :: Lens' DescribeIdentityResponse (Maybe UTCTime)
dirCreationDate = lens _dirCreationDate (\s a -> s { _dirCreationDate = a }) . mapping _Time

-- | A unique identifier in the format REGION:GUID.
dirIdentityId :: Lens' DescribeIdentityResponse (Maybe Text)
dirIdentityId = lens _dirIdentityId (\s a -> s { _dirIdentityId = a })

-- | Date on which the identity was last modified.
dirLastModifiedDate :: Lens' DescribeIdentityResponse (Maybe UTCTime)
dirLastModifiedDate =
    lens _dirLastModifiedDate (\s a -> s { _dirLastModifiedDate = a })
        . mapping _Time

-- | A set of optional name-value pairs that map provider names to provider tokens.
dirLogins :: Lens' DescribeIdentityResponse [Text]
dirLogins = lens _dirLogins (\s a -> s { _dirLogins = a }) . _List

instance ToPath DescribeIdentity where
    toPath = const "/"

instance ToQuery DescribeIdentity where
    toQuery = const mempty

instance ToHeaders DescribeIdentity

instance ToJSON DescribeIdentity where
    toJSON DescribeIdentity{..} = object
        [ "IdentityId" .= _diIdentityId
        ]

instance AWSRequest DescribeIdentity where
    type Sv DescribeIdentity = CognitoIdentity
    type Rs DescribeIdentity = DescribeIdentityResponse

    request  = post "DescribeIdentity"
    response = jsonResponse

instance FromJSON DescribeIdentityResponse where
    parseJSON = withObject "DescribeIdentityResponse" $ \o -> DescribeIdentityResponse
        <$> o .:? "CreationDate"
        <*> o .:? "IdentityId"
        <*> o .:? "LastModifiedDate"
        <*> o .:? "Logins" .!= mempty
