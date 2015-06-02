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

-- Module      : Network.AWS.IAM.GetAccessKeyLastUsed
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

-- | Retrieves information about when the specified access key was last used. The
-- information includes the date and time of last use, along with the AWS
-- service and region that were specified in the last request made with that key.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccessKeyLastUsed.html>
module Network.AWS.IAM.GetAccessKeyLastUsed
    (
    -- * Request
      GetAccessKeyLastUsed
    -- ** Request constructor
    , getAccessKeyLastUsed
    -- ** Request lenses
    , gakluAccessKeyId

    -- * Response
    , GetAccessKeyLastUsedResponse
    -- ** Response constructor
    , getAccessKeyLastUsedResponse
    -- ** Response lenses
    , gaklurAccessKeyLastUsed
    , gaklurUserName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype GetAccessKeyLastUsed = GetAccessKeyLastUsed
    { _gakluAccessKeyId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetAccessKeyLastUsed' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gakluAccessKeyId' @::@ 'Text'
--
getAccessKeyLastUsed :: Text -- ^ 'gakluAccessKeyId'
                     -> GetAccessKeyLastUsed
getAccessKeyLastUsed p1 = GetAccessKeyLastUsed
    { _gakluAccessKeyId = p1
    }

-- | The identifier of an access key.
gakluAccessKeyId :: Lens' GetAccessKeyLastUsed Text
gakluAccessKeyId = lens _gakluAccessKeyId (\s a -> s { _gakluAccessKeyId = a })

data GetAccessKeyLastUsedResponse = GetAccessKeyLastUsedResponse
    { _gaklurAccessKeyLastUsed :: Maybe AccessKeyLastUsed
    , _gaklurUserName          :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'GetAccessKeyLastUsedResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaklurAccessKeyLastUsed' @::@ 'Maybe' 'AccessKeyLastUsed'
--
-- * 'gaklurUserName' @::@ 'Maybe' 'Text'
--
getAccessKeyLastUsedResponse :: GetAccessKeyLastUsedResponse
getAccessKeyLastUsedResponse = GetAccessKeyLastUsedResponse
    { _gaklurUserName          = Nothing
    , _gaklurAccessKeyLastUsed = Nothing
    }

-- | Contains information about the last time the access key was used.
gaklurAccessKeyLastUsed :: Lens' GetAccessKeyLastUsedResponse (Maybe AccessKeyLastUsed)
gaklurAccessKeyLastUsed =
    lens _gaklurAccessKeyLastUsed (\s a -> s { _gaklurAccessKeyLastUsed = a })

-- | The name of the AWS IAM user that owns this access key.
gaklurUserName :: Lens' GetAccessKeyLastUsedResponse (Maybe Text)
gaklurUserName = lens _gaklurUserName (\s a -> s { _gaklurUserName = a })

instance ToPath GetAccessKeyLastUsed where
    toPath = const "/"

instance ToQuery GetAccessKeyLastUsed where
    toQuery GetAccessKeyLastUsed{..} = mconcat
        [ "AccessKeyId" =? _gakluAccessKeyId
        ]

instance ToHeaders GetAccessKeyLastUsed

instance AWSRequest GetAccessKeyLastUsed where
    type Sv GetAccessKeyLastUsed = IAM
    type Rs GetAccessKeyLastUsed = GetAccessKeyLastUsedResponse

    request  = post "GetAccessKeyLastUsed"
    response = xmlResponse

instance FromXML GetAccessKeyLastUsedResponse where
    parseXML = withElement "GetAccessKeyLastUsedResult" $ \x -> GetAccessKeyLastUsedResponse
        <$> x .@? "AccessKeyLastUsed"
        <*> x .@? "UserName"
