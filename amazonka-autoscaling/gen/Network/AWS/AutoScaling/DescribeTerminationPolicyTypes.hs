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

-- Module      : Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of all termination policies supported by Auto Scaling.
module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
    (
    -- * Request
      DescribeTerminationPolicyTypes
    -- ** Request constructor
    , describeTerminationPolicyTypes

    -- * Response
    , DescribeTerminationPolicyTypesResponse
    -- ** Response constructor
    , describeTerminationPolicyTypesResponse
    -- ** Response lenses
    , dtptrTerminationPolicyTypes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeTerminationPolicyTypes' constructor.
describeTerminationPolicyTypes :: DescribeTerminationPolicyTypes
describeTerminationPolicyTypes = DescribeTerminationPolicyTypes

instance ToQuery DescribeTerminationPolicyTypes

instance ToPath DescribeTerminationPolicyTypes where
    toPath = const "/"

newtype DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse
    { _dtptrTerminationPolicyTypes :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeTerminationPolicyTypesResponse where
    type Item DescribeTerminationPolicyTypesResponse = Text

    fromList = DescribeTerminationPolicyTypesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dtptrTerminationPolicyTypes

-- | 'DescribeTerminationPolicyTypesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtptrTerminationPolicyTypes' @::@ ['Text']
--
describeTerminationPolicyTypesResponse :: DescribeTerminationPolicyTypesResponse
describeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse
    { _dtptrTerminationPolicyTypes = mempty
    }

-- | Termination policies supported by Auto Scaling. They are: OldestInstance,
-- OldestLaunchConfiguration, NewestInstance, ClosestToNextInstanceHour,
-- Default.
dtptrTerminationPolicyTypes :: Lens' DescribeTerminationPolicyTypesResponse [Text]
dtptrTerminationPolicyTypes =
    lens _dtptrTerminationPolicyTypes
        (\s a -> s { _dtptrTerminationPolicyTypes = a })

instance AWSRequest DescribeTerminationPolicyTypes where
    type Sv DescribeTerminationPolicyTypes = AutoScaling
    type Rs DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypesResponse

    request  = post "DescribeTerminationPolicyTypes"
    response = xmlResponse $ \h x -> DescribeTerminationPolicyTypesResponse
        <$> x %| "TerminationPolicyTypes"
