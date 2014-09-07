{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeServiceErrors
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
module Network.AWS.OpsWorks.V2013_02_18.DescribeServiceErrors
    (
    -- * Request
      DescribeServiceErrors
    -- ** Request constructor
    , mkDescribeServiceErrors
    -- ** Request lenses
    , dseStackId
    , dseInstanceId
    , dseServiceErrorIds

    -- * Response
    , DescribeServiceErrorsResponse
    -- ** Response lenses
    , dsersServiceErrors
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data DescribeServiceErrors = DescribeServiceErrors
    { _dseStackId :: Maybe Text
    , _dseInstanceId :: Maybe Text
    , _dseServiceErrorIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeServiceErrors' request.
mkDescribeServiceErrors :: DescribeServiceErrors
mkDescribeServiceErrors = DescribeServiceErrors
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
    { _dsersServiceErrors :: [ServiceError]
    } deriving (Show, Generic)

-- | An array of ServiceError objects that describe the specified service
-- errors.
dsersServiceErrors :: Lens' DescribeServiceErrorsResponse [ServiceError]
dsersServiceErrors =
    lens _dsersServiceErrors (\s a -> s { _dsersServiceErrors = a })

instance FromJSON DescribeServiceErrorsResponse

instance AWSRequest DescribeServiceErrors where
    type Sv DescribeServiceErrors = OpsWorks
    type Rs DescribeServiceErrors = DescribeServiceErrorsResponse

    request = get
    response _ = jsonResponse
