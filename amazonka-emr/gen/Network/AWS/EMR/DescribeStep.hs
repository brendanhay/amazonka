{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.EMR.DescribeStep
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides more detail about the cluster step.
module Network.AWS.EMR.DescribeStep
    (
    -- * Request
      DescribeStep
    -- ** Request constructor
    , describeStep
    -- ** Request lenses
    , dsClusterId
    , dsStepId

    -- * Response
    , DescribeStepResponse
    -- ** Response constructor
    , describeStepResponse
    -- ** Response lenses
    , dsrStep
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.EMR.Types

data DescribeStep = DescribeStep
    { _dsClusterId :: Text
    , _dsStepId    :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeStep' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsClusterId' @::@ 'Text'
--
-- * 'dsStepId' @::@ 'Text'
--
describeStep :: Text -- ^ 'dsClusterId'
             -> Text -- ^ 'dsStepId'
             -> DescribeStep
describeStep p1 p2 = DescribeStep
    { _dsClusterId = p1
    , _dsStepId    = p2
    }

-- | The identifier of the cluster with steps to describe.
dsClusterId :: Lens' DescribeStep Text
dsClusterId = lens _dsClusterId (\s a -> s { _dsClusterId = a })

-- | The identifier of the step to describe.
dsStepId :: Lens' DescribeStep Text
dsStepId = lens _dsStepId (\s a -> s { _dsStepId = a })

instance ToPath DescribeStep where
    toPath = const "/"

instance ToQuery DescribeStep where
    toQuery = const mempty

instance ToHeaders DescribeStep

instance ToBody DescribeStep where
    toBody = toBody . encode . _dsClusterId

newtype DescribeStepResponse = DescribeStepResponse
    { _dsrStep :: Maybe Step
    } deriving (Eq, Show, Generic)

-- | 'DescribeStepResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrStep' @::@ 'Maybe' 'Step'
--
describeStepResponse :: DescribeStepResponse
describeStepResponse = DescribeStepResponse
    { _dsrStep = Nothing
    }

-- | The step details for the requested step identifier.
dsrStep :: Lens' DescribeStepResponse (Maybe Step)
dsrStep = lens _dsrStep (\s a -> s { _dsrStep = a })

instance AWSRequest DescribeStep where
    type Sv DescribeStep = EMR
    type Rs DescribeStep = DescribeStepResponse

    request  = post
    response = jsonResponse $ \h o -> DescribeStepResponse
        <$> o .: "Step"
