{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Swaps the CNAMEs of two environments.
-- https://elasticbeanstalk.us-east-1.amazon.com/?SourceEnvironmentName=SampleApp
-- &DestinationEnvironmentName=SampleApp2 &Operation=SwapEnvironmentCNAMEs
-- &AuthParams f4e1b145-9080-11e0-8e5a-a558e0ce1fc4.
module Network.AWS.ElasticBeanstalk
    (
    -- * Request
      SwapEnvironmentCNAMEs
    -- ** Request constructor
    , mkSwapEnvironmentCNAMEs
    -- ** Request lenses
    , secnameSourceEnvironmentId
    , secnameSourceEnvironmentName
    , secnameDestinationEnvironmentId
    , secnameDestinationEnvironmentName

    -- * Response
    , SwapEnvironmentCNAMEsResponse
    -- ** Response constructor
    , mkSwapEnvironmentCNAMEsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | 
data SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEs
    { _secnameSourceEnvironmentId :: !(Maybe Text)
    , _secnameSourceEnvironmentName :: !(Maybe Text)
    , _secnameDestinationEnvironmentId :: !(Maybe Text)
    , _secnameDestinationEnvironmentName :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SwapEnvironmentCNAMEs' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceEnvironmentId ::@ @Maybe Text@
--
-- * @SourceEnvironmentName ::@ @Maybe Text@
--
-- * @DestinationEnvironmentId ::@ @Maybe Text@
--
-- * @DestinationEnvironmentName ::@ @Maybe Text@
--
mkSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEs
mkSwapEnvironmentCNAMEs = SwapEnvironmentCNAMEs
    { _secnameSourceEnvironmentId = Nothing
    , _secnameSourceEnvironmentName = Nothing
    , _secnameDestinationEnvironmentId = Nothing
    , _secnameDestinationEnvironmentName = Nothing
    }

-- | The ID of the source environment. Condition: You must specify at least the
-- SourceEnvironmentID or the SourceEnvironmentName. You may also specify
-- both. If you specify the SourceEnvironmentId, you must specify the
-- DestinationEnvironmentId.
secnameSourceEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameSourceEnvironmentId =
    lens _secnameSourceEnvironmentId
         (\s a -> s { _secnameSourceEnvironmentId = a })

-- | The name of the source environment. Condition: You must specify at least
-- the SourceEnvironmentID or the SourceEnvironmentName. You may also specify
-- both. If you specify the SourceEnvironmentName, you must specify the
-- DestinationEnvironmentName.
secnameSourceEnvironmentName :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameSourceEnvironmentName =
    lens _secnameSourceEnvironmentName
         (\s a -> s { _secnameSourceEnvironmentName = a })

-- | The ID of the destination environment. Condition: You must specify at least
-- the DestinationEnvironmentID or the DestinationEnvironmentName. You may
-- also specify both. You must specify the SourceEnvironmentId with the
-- DestinationEnvironmentId.
secnameDestinationEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameDestinationEnvironmentId =
    lens _secnameDestinationEnvironmentId
         (\s a -> s { _secnameDestinationEnvironmentId = a })

-- | The name of the destination environment. Condition: You must specify at
-- least the DestinationEnvironmentID or the DestinationEnvironmentName. You
-- may also specify both. You must specify the SourceEnvironmentName with the
-- DestinationEnvironmentName.
secnameDestinationEnvironmentName :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameDestinationEnvironmentName =
    lens _secnameDestinationEnvironmentName
         (\s a -> s { _secnameDestinationEnvironmentName = a })

instance ToQuery SwapEnvironmentCNAMEs where
    toQuery = genericQuery def

data SwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SwapEnvironmentCNAMEsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkSwapEnvironmentCNAMEsResponse :: SwapEnvironmentCNAMEsResponse
mkSwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse

instance AWSRequest SwapEnvironmentCNAMEs where
    type Sv SwapEnvironmentCNAMEs = ElasticBeanstalk
    type Rs SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEsResponse

    request = post "SwapEnvironmentCNAMEs"
    response _ = nullaryResponse SwapEnvironmentCNAMEsResponse
