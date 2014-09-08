{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeTerminationPolicyTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of all termination policies supported by Auto Scaling.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeTerminationPolicyTypes &AUTHPARAMS
-- ClosestToNextInstanceHour Default NewestInstance OldestInstance
-- OldestLaunchConfiguration d9a05827-b735-11e2-a40c-c79a5EXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.DescribeTerminationPolicyTypes
    (
    -- * Request
      DescribeTerminationPolicyTypes
    -- ** Request constructor
    , mkDescribeTerminationPolicyTypes
    -- * Response
    , DescribeTerminationPolicyTypesResponse
    -- ** Response constructor
    , mkDescribeTerminationPolicyTypesResponse
    -- ** Response lenses
    , dtptrTerminationPolicyTypes
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTerminationPolicyTypes' request.
mkDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypes
mkDescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes

instance ToQuery DescribeTerminationPolicyTypes where
    toQuery = genericQuery def

-- | The TerminationPolicyTypes data type.
newtype DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse
    { _dtptrTerminationPolicyTypes :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTerminationPolicyTypesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeTerminationPolicyTypesResponse :: DescribeTerminationPolicyTypesResponse
mkDescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse
    { _dtptrTerminationPolicyTypes = mempty
    }

-- | Termination policies supported by Auto Scaling. They are: OldestInstance,
-- OldestLaunchConfiguration, NewestInstance, ClosestToNextInstanceHour,
-- Default.
dtptrTerminationPolicyTypes :: Lens' DescribeTerminationPolicyTypesResponse [Text]
dtptrTerminationPolicyTypes =
    lens _dtptrTerminationPolicyTypes
         (\s a -> s { _dtptrTerminationPolicyTypes = a })

instance FromXML DescribeTerminationPolicyTypesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeTerminationPolicyTypes where
    type Sv DescribeTerminationPolicyTypes = AutoScaling
    type Rs DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypesResponse

    request = post "DescribeTerminationPolicyTypes"
    response _ = xmlResponse
