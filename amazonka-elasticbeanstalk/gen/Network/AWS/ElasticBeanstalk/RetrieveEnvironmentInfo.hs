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

-- Module      : Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the compiled information from a 'RequestEnvironmentInfo' request.
-- Related Topics 'RequestEnvironmentInfo'.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_RetrieveEnvironmentInfo.html>
module Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
    (
    -- * Request
      RetrieveEnvironmentInfo
    -- ** Request constructor
    , retrieveEnvironmentInfo
    -- ** Request lenses
    , rei1EnvironmentId
    , rei1EnvironmentName
    , rei1InfoType

    -- * Response
    , RetrieveEnvironmentInfoResponse
    -- ** Response constructor
    , retrieveEnvironmentInfoResponse
    -- ** Response lenses
    , reirEnvironmentInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data RetrieveEnvironmentInfo = RetrieveEnvironmentInfo
    { _rei1EnvironmentId   :: Maybe Text
    , _rei1EnvironmentName :: Maybe Text
    , _rei1InfoType        :: EnvironmentInfoType
    } deriving (Eq, Show)

-- | 'RetrieveEnvironmentInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rei1EnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'rei1EnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'rei1InfoType' @::@ 'EnvironmentInfoType'
--
retrieveEnvironmentInfo :: EnvironmentInfoType -- ^ 'rei1InfoType'
                        -> RetrieveEnvironmentInfo
retrieveEnvironmentInfo p1 = RetrieveEnvironmentInfo
    { _rei1InfoType        = p1
    , _rei1EnvironmentId   = Nothing
    , _rei1EnvironmentName = Nothing
    }

-- | The ID of the data's environment. If no such environment is found,
-- returns an 'InvalidParameterValue' error. Condition: You must specify
-- either this or an EnvironmentName, or both. If you do not specify either,
-- AWS Elastic Beanstalk returns 'MissingRequiredParameter' error.
rei1EnvironmentId :: Lens' RetrieveEnvironmentInfo (Maybe Text)
rei1EnvironmentId =
    lens _rei1EnvironmentId (\s a -> s { _rei1EnvironmentId = a })

-- | The name of the data's environment. If no such environment is found,
-- returns an 'InvalidParameterValue' error. Condition: You must specify
-- either this or an EnvironmentId, or both. If you do not specify either,
-- AWS Elastic Beanstalk returns 'MissingRequiredParameter' error.
rei1EnvironmentName :: Lens' RetrieveEnvironmentInfo (Maybe Text)
rei1EnvironmentName =
    lens _rei1EnvironmentName (\s a -> s { _rei1EnvironmentName = a })

-- | The type of information to retrieve.
rei1InfoType :: Lens' RetrieveEnvironmentInfo EnvironmentInfoType
rei1InfoType = lens _rei1InfoType (\s a -> s { _rei1InfoType = a })

newtype RetrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse
    { _reirEnvironmentInfo :: List "EnvironmentInfo" EnvironmentInfoDescription
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList RetrieveEnvironmentInfoResponse where
    type Item RetrieveEnvironmentInfoResponse = EnvironmentInfoDescription

    fromList = RetrieveEnvironmentInfoResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _reirEnvironmentInfo

-- | 'RetrieveEnvironmentInfoResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reirEnvironmentInfo' @::@ ['EnvironmentInfoDescription']
--
retrieveEnvironmentInfoResponse :: RetrieveEnvironmentInfoResponse
retrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse
    { _reirEnvironmentInfo = mempty
    }

-- | The 'EnvironmentInfoDescription' of the environment.
reirEnvironmentInfo :: Lens' RetrieveEnvironmentInfoResponse [EnvironmentInfoDescription]
reirEnvironmentInfo =
    lens _reirEnvironmentInfo (\s a -> s { _reirEnvironmentInfo = a })
        . _List

instance ToPath RetrieveEnvironmentInfo where
    toPath = const "/"

instance ToQuery RetrieveEnvironmentInfo where
    toQuery RetrieveEnvironmentInfo{..} = mconcat
        [ "EnvironmentId"   =? _rei1EnvironmentId
        , "EnvironmentName" =? _rei1EnvironmentName
        , "InfoType"        =? _rei1InfoType
        ]

instance ToHeaders RetrieveEnvironmentInfo

instance AWSRequest RetrieveEnvironmentInfo where
    type Sv RetrieveEnvironmentInfo = ElasticBeanstalk
    type Rs RetrieveEnvironmentInfo = RetrieveEnvironmentInfoResponse

    request  = post "RetrieveEnvironmentInfo"
    response = xmlResponse

instance FromXML RetrieveEnvironmentInfoResponse where
    parseXML = withElement "RetrieveEnvironmentInfoResult" $ \x -> RetrieveEnvironmentInfoResponse
        <$> x .@  "EnvironmentInfo"
