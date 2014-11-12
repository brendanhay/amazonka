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

-- Module      : Network.AWS.Redshift.DescribeDefaultClusterParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of parameter settings for the specified parameter group
-- family. For more information about managing parameter groups, go to Amazon
-- Redshift Parameter Groups in the Amazon Redshift Management Guide.
module Network.AWS.Redshift.DescribeDefaultClusterParameters
    (
    -- * Request
      DescribeDefaultClusterParametersMessage
    -- ** Request constructor
    , describeDefaultClusterParameters
    -- ** Request lenses
    , ddcpmMarker
    , ddcpmMaxRecords
    , ddcpmParameterGroupFamily

    -- * Response
    , DescribeDefaultClusterParametersResult
    -- ** Response constructor
    , describeDefaultClusterParametersResponse
    -- ** Response lenses
    , ddcprDefaultClusterParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeDefaultClusterParametersMessage = DescribeDefaultClusterParametersMessage
    { _ddcpmMarker               :: Maybe Text
    , _ddcpmMaxRecords           :: Maybe Int
    , _ddcpmParameterGroupFamily :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeDefaultClusterParametersMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcpmMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddcpmMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'ddcpmParameterGroupFamily' @::@ 'Text'
--
describeDefaultClusterParameters :: Text -- ^ 'ddcpmParameterGroupFamily'
                                 -> DescribeDefaultClusterParametersMessage
describeDefaultClusterParameters p1 = DescribeDefaultClusterParametersMessage
    { _ddcpmParameterGroupFamily = p1
    , _ddcpmMaxRecords           = Nothing
    , _ddcpmMarker               = Nothing
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeDefaultClusterParameters request exceed the value specified in
-- MaxRecords, AWS returns a value in the Marker field of the response. You
-- can retrieve the next set of response records by providing the returned
-- marker value in the Marker parameter and retrying the request.
ddcpmMarker :: Lens' DescribeDefaultClusterParametersMessage (Maybe Text)
ddcpmMarker = lens _ddcpmMarker (\s a -> s { _ddcpmMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
ddcpmMaxRecords :: Lens' DescribeDefaultClusterParametersMessage (Maybe Int)
ddcpmMaxRecords = lens _ddcpmMaxRecords (\s a -> s { _ddcpmMaxRecords = a })

-- | The name of the cluster parameter group family.
ddcpmParameterGroupFamily :: Lens' DescribeDefaultClusterParametersMessage Text
ddcpmParameterGroupFamily =
    lens _ddcpmParameterGroupFamily
        (\s a -> s { _ddcpmParameterGroupFamily = a })

instance ToQuery DescribeDefaultClusterParametersMessage

instance ToPath DescribeDefaultClusterParametersMessage where
    toPath = const "/"

newtype DescribeDefaultClusterParametersResult = DescribeDefaultClusterParametersResult
    { _ddcprDefaultClusterParameters :: Maybe DefaultClusterParameters
    } deriving (Eq, Show, Generic)

-- | 'DescribeDefaultClusterParametersResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcprDefaultClusterParameters' @::@ 'Maybe' 'DefaultClusterParameters'
--
describeDefaultClusterParametersResponse :: DescribeDefaultClusterParametersResult
describeDefaultClusterParametersResponse = DescribeDefaultClusterParametersResult
    { _ddcprDefaultClusterParameters = Nothing
    }

ddcprDefaultClusterParameters :: Lens' DescribeDefaultClusterParametersResult (Maybe DefaultClusterParameters)
ddcprDefaultClusterParameters =
    lens _ddcprDefaultClusterParameters
        (\s a -> s { _ddcprDefaultClusterParameters = a })

instance FromXML DescribeDefaultClusterParametersResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeDefaultClusterParametersResult"

instance AWSRequest DescribeDefaultClusterParametersMessage where
    type Sv DescribeDefaultClusterParametersMessage = Redshift
    type Rs DescribeDefaultClusterParametersMessage = DescribeDefaultClusterParametersResult

    request  = post "DescribeDefaultClusterParameters"
    response = xmlResponse $ \h x -> DescribeDefaultClusterParametersResult
        <$> x %| "DefaultClusterParameters"
