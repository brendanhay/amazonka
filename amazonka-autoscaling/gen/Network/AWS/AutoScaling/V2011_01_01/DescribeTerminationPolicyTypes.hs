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
    , mkUnknown
    -- * Response
    , DescribeTerminationPolicyTypesResponse
    -- ** Response lenses
    , dtptaTerminationPolicyTypes
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTerminationPolicyTypes' request.
mkUnknown :: DescribeTerminationPolicyTypes
mkUnknown = DescribeTerminationPolicyTypes
{-# INLINE mkUnknown #-}

data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes
    deriving (Eq, Show, Generic)

instance ToQuery DescribeTerminationPolicyTypes where
    toQuery = genericQuery def

newtype DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse
    { _dtptaTerminationPolicyTypes :: [Text]
      -- ^ Termination policies supported by Auto Scaling. They are:
      -- OldestInstance, OldestLaunchConfiguration, NewestInstance,
      -- ClosestToNextInstanceHour, Default.
    } deriving (Show, Generic)

-- | Termination policies supported by Auto Scaling. They are: OldestInstance,
-- OldestLaunchConfiguration, NewestInstance, ClosestToNextInstanceHour,
-- Default.
dtptaTerminationPolicyTypes :: Lens' DescribeTerminationPolicyTypesResponse ([Text])
dtptaTerminationPolicyTypes = lens _dtptaTerminationPolicyTypes (\s a -> s { _dtptaTerminationPolicyTypes = a })
{-# INLINE dtptaTerminationPolicyTypes #-}

instance FromXML DescribeTerminationPolicyTypesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeTerminationPolicyTypes where
    type Sv DescribeTerminationPolicyTypes = AutoScaling
    type Rs DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypesResponse

    request = post "DescribeTerminationPolicyTypes"
    response _ = xmlResponse
