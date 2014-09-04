{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.SwapEnvironmentCNAMEs
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.SwapEnvironmentCNAMEs
    (
    -- * Request
      SwapEnvironmentCNAMEs
    -- ** Request constructor
    , mkSwapEnvironmentCNAMEsMessage
    -- ** Request lenses
    , secnamemSourceEnvironmentId
    , secnamemSourceEnvironmentName
    , secnamemDestinationEnvironmentId
    , secnamemDestinationEnvironmentName

    -- * Response
    , SwapEnvironmentCNAMEsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SwapEnvironmentCNAMEs' request.
mkSwapEnvironmentCNAMEsMessage :: SwapEnvironmentCNAMEs
mkSwapEnvironmentCNAMEsMessage = SwapEnvironmentCNAMEs
    { _secnamemSourceEnvironmentId = Nothing
    , _secnamemSourceEnvironmentName = Nothing
    , _secnamemDestinationEnvironmentId = Nothing
    , _secnamemDestinationEnvironmentName = Nothing
    }
{-# INLINE mkSwapEnvironmentCNAMEsMessage #-}

data SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEs
    { _secnamemSourceEnvironmentId :: Maybe Text
      -- ^ The ID of the source environment. Condition: You must specify at
      -- least the SourceEnvironmentID or the SourceEnvironmentName. You
      -- may also specify both. If you specify the SourceEnvironmentId,
      -- you must specify the DestinationEnvironmentId.
    , _secnamemSourceEnvironmentName :: Maybe Text
      -- ^ The name of the source environment. Condition: You must specify
      -- at least the SourceEnvironmentID or the SourceEnvironmentName.
      -- You may also specify both. If you specify the
      -- SourceEnvironmentName, you must specify the
      -- DestinationEnvironmentName.
    , _secnamemDestinationEnvironmentId :: Maybe Text
      -- ^ The ID of the destination environment. Condition: You must
      -- specify at least the DestinationEnvironmentID or the
      -- DestinationEnvironmentName. You may also specify both. You must
      -- specify the SourceEnvironmentId with the
      -- DestinationEnvironmentId.
    , _secnamemDestinationEnvironmentName :: Maybe Text
      -- ^ The name of the destination environment. Condition: You must
      -- specify at least the DestinationEnvironmentID or the
      -- DestinationEnvironmentName. You may also specify both. You must
      -- specify the SourceEnvironmentName with the
      -- DestinationEnvironmentName.
    } deriving (Show, Generic)

-- | The ID of the source environment. Condition: You must specify at least the
-- SourceEnvironmentID or the SourceEnvironmentName. You may also specify
-- both. If you specify the SourceEnvironmentId, you must specify the
-- DestinationEnvironmentId.
secnamemSourceEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnamemSourceEnvironmentId = lens _secnamemSourceEnvironmentId (\s a -> s { _secnamemSourceEnvironmentId = a })
{-# INLINE secnamemSourceEnvironmentId #-}

-- | The name of the source environment. Condition: You must specify at least
-- the SourceEnvironmentID or the SourceEnvironmentName. You may also specify
-- both. If you specify the SourceEnvironmentName, you must specify the
-- DestinationEnvironmentName.
secnamemSourceEnvironmentName :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnamemSourceEnvironmentName = lens _secnamemSourceEnvironmentName (\s a -> s { _secnamemSourceEnvironmentName = a })
{-# INLINE secnamemSourceEnvironmentName #-}

-- | The ID of the destination environment. Condition: You must specify at least
-- the DestinationEnvironmentID or the DestinationEnvironmentName. You may
-- also specify both. You must specify the SourceEnvironmentId with the
-- DestinationEnvironmentId.
secnamemDestinationEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnamemDestinationEnvironmentId = lens _secnamemDestinationEnvironmentId (\s a -> s { _secnamemDestinationEnvironmentId = a })
{-# INLINE secnamemDestinationEnvironmentId #-}

-- | The name of the destination environment. Condition: You must specify at
-- least the DestinationEnvironmentID or the DestinationEnvironmentName. You
-- may also specify both. You must specify the SourceEnvironmentName with the
-- DestinationEnvironmentName.
secnamemDestinationEnvironmentName :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnamemDestinationEnvironmentName = lens _secnamemDestinationEnvironmentName (\s a -> s { _secnamemDestinationEnvironmentName = a })
{-# INLINE secnamemDestinationEnvironmentName #-}

instance ToQuery SwapEnvironmentCNAMEs where
    toQuery = genericQuery def

data SwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SwapEnvironmentCNAMEs where
    type Sv SwapEnvironmentCNAMEs = ElasticBeanstalk
    type Rs SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEsResponse

    request = post "SwapEnvironmentCNAMEs"
    response _ = nullaryResponse SwapEnvironmentCNAMEsResponse
