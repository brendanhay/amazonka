{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeServiceErrors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes AWS OpsWorks service errors. Required Permissions: To use this
-- action, an IAM user must have a Show, Deploy, or Manage permissions level
-- for the stack, or an attached policy that explicitly grants permissions.
-- For more information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeServiceErrors
    (
    -- * Request
      DescribeServiceErrors
    -- ** Request constructor
    , describeServiceErrors
    -- ** Request lenses
    , dseStackId
    , dseInstanceId
    , dseServiceErrorIds

    -- * Response
    , DescribeServiceErrorsResponse
    -- ** Response constructor
    , describeServiceErrorsResponse
    -- ** Response lenses
    , dserServiceErrors
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeServiceErrors = DescribeServiceErrors
    { _dseStackId :: Maybe Text
    , _dseInstanceId :: Maybe Text
    , _dseServiceErrorIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeServiceErrors' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @ServiceErrorIds ::@ @[Text]@
--
describeServiceErrors :: DescribeServiceErrors
describeServiceErrors = DescribeServiceErrors
    { _dseStackId = Nothing
    , _dseInstanceId = Nothing
    , _dseServiceErrorIds = mempty
    }

-- | The stack ID. If you use this parameter, DescribeServiceErrors returns
-- descriptions of the errors associated with the specified stack.
dseStackId :: Lens' DescribeServiceErrors (Maybe Text)
dseStackId = lens _dseStackId (\s a -> s { _dseStackId = a })

-- | The instance ID. If you use this parameter, DescribeServiceErrors returns
-- descriptions of the errors associated with the specified instance.
dseInstanceId :: Lens' DescribeServiceErrors (Maybe Text)
dseInstanceId = lens _dseInstanceId (\s a -> s { _dseInstanceId = a })

-- | An array of service error IDs. If you use this parameter,
-- DescribeServiceErrors returns descriptions of the specified errors.
-- Otherwise, it returns a description of every error.
dseServiceErrorIds :: Lens' DescribeServiceErrors [Text]
dseServiceErrorIds =
    lens _dseServiceErrorIds (\s a -> s { _dseServiceErrorIds = a })

instance ToPath DescribeServiceErrors

instance ToQuery DescribeServiceErrors

instance ToHeaders DescribeServiceErrors

instance ToJSON DescribeServiceErrors

-- | Contains the response to a DescribeServiceErrors request.
newtype DescribeServiceErrorsResponse = DescribeServiceErrorsResponse
    { _dserServiceErrors :: [ServiceError]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeServiceErrorsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ServiceErrors ::@ @[ServiceError]@
--
describeServiceErrorsResponse :: DescribeServiceErrorsResponse
describeServiceErrorsResponse = DescribeServiceErrorsResponse
    { _dserServiceErrors = mempty
    }

-- | An array of ServiceError objects that describe the specified service
-- errors.
dserServiceErrors :: Lens' DescribeServiceErrorsResponse [ServiceError]
dserServiceErrors =
    lens _dserServiceErrors (\s a -> s { _dserServiceErrors = a })

instance FromJSON DescribeServiceErrorsResponse

instance AWSRequest DescribeServiceErrors where
    type Sv DescribeServiceErrors = OpsWorks
    type Rs DescribeServiceErrors = DescribeServiceErrorsResponse

    request = get
    response _ = jsonResponse
