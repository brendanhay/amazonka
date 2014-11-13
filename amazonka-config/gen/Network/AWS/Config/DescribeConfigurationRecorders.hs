{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Config.Types

newtype DescribeConfigurationRecorders = DescribeConfigurationRecorders
    { _dcrConfigurationRecorderNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

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

instance ToPath DescribeConfigurationRecorders where
    toPath = const "/"

instance ToQuery DescribeConfigurationRecorders where
    toQuery = const mempty

instance ToHeaders DescribeConfigurationRecorders

instance ToBody DescribeConfigurationRecorders where
    toBody = toBody . encode . _dcrConfigurationRecorderNames

newtype DescribeConfigurationRecordersResponse = DescribeConfigurationRecordersResponse
    { _dcrrConfigurationRecorders :: [ConfigurationRecorder]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

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

-- FromJSON

instance AWSRequest DescribeConfigurationRecorders where
    type Sv DescribeConfigurationRecorders = Config
    type Rs DescribeConfigurationRecorders = DescribeConfigurationRecordersResponse

    request  = post'
    response = jsonResponse $ \h o -> DescribeConfigurationRecordersResponse
        <$> o .: "ConfigurationRecorders"
