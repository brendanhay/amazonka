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

-- Module      : Network.AWS.Config.DescribeConfigurationRecorders
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the name of one or more specified configuration recorders. If the
-- recorder name is not specified, this action returns the names of all the
-- configuration recorders associated with the account.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_DescribeConfigurationRecorders.html>
module Network.AWS.Config.DescribeConfigurationRecorders
    (
    -- * Request
      DescribeConfigurationRecorders
    -- ** Request constructor
    , describeConfigurationRecorders
    -- ** Request lenses
    , dcrConfigurationRecorderNames

    -- * Response
    , DescribeConfigurationRecordersResponse
    -- ** Response constructor
    , describeConfigurationRecordersResponse
    -- ** Response lenses
    , dcrrConfigurationRecorders
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Config.Types
import qualified GHC.Exts

newtype DescribeConfigurationRecorders = DescribeConfigurationRecorders
    { _dcrConfigurationRecorderNames :: List "ConfigurationRecorderNames" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeConfigurationRecorders where
    type Item DescribeConfigurationRecorders = Text

    fromList = DescribeConfigurationRecorders . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dcrConfigurationRecorderNames

-- | 'DescribeConfigurationRecorders' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrConfigurationRecorderNames' @::@ ['Text']
--
describeConfigurationRecorders :: DescribeConfigurationRecorders
describeConfigurationRecorders = DescribeConfigurationRecorders
    { _dcrConfigurationRecorderNames = mempty
    }

-- | A list of configuration recorder names.
dcrConfigurationRecorderNames :: Lens' DescribeConfigurationRecorders [Text]
dcrConfigurationRecorderNames =
    lens _dcrConfigurationRecorderNames
        (\s a -> s { _dcrConfigurationRecorderNames = a })
            . _List

newtype DescribeConfigurationRecordersResponse = DescribeConfigurationRecordersResponse
    { _dcrrConfigurationRecorders :: List "ConfigurationRecorders" ConfigurationRecorder
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeConfigurationRecordersResponse where
    type Item DescribeConfigurationRecordersResponse = ConfigurationRecorder

    fromList = DescribeConfigurationRecordersResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dcrrConfigurationRecorders

-- | 'DescribeConfigurationRecordersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrrConfigurationRecorders' @::@ ['ConfigurationRecorder']
--
describeConfigurationRecordersResponse :: DescribeConfigurationRecordersResponse
describeConfigurationRecordersResponse = DescribeConfigurationRecordersResponse
    { _dcrrConfigurationRecorders = mempty
    }

-- | A list that contains the descriptions of the specified configuration
-- recorders.
dcrrConfigurationRecorders :: Lens' DescribeConfigurationRecordersResponse [ConfigurationRecorder]
dcrrConfigurationRecorders =
    lens _dcrrConfigurationRecorders
        (\s a -> s { _dcrrConfigurationRecorders = a })
            . _List

instance ToPath DescribeConfigurationRecorders where
    toPath = const "/"

instance ToQuery DescribeConfigurationRecorders where
    toQuery = const mempty

instance ToHeaders DescribeConfigurationRecorders

instance ToJSON DescribeConfigurationRecorders where
    toJSON DescribeConfigurationRecorders{..} = object
        [ "ConfigurationRecorderNames" .= _dcrConfigurationRecorderNames
        ]

json

instance AWSRequest DescribeConfigurationRecorders where
    type Sv DescribeConfigurationRecorders = Config
    type Rs DescribeConfigurationRecorders = DescribeConfigurationRecordersResponse

    request  = post "DescribeConfigurationRecorders"
    response = jsonResponse

instance FromJSON DescribeConfigurationRecordersResponse where
    parseJSON = withObject "DescribeConfigurationRecordersResponse" $ \o -> DescribeConfigurationRecordersResponse
        <$> o .:  "ConfigurationRecorders"
