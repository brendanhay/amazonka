{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.OpsWorks.V2013_02_18.DescribeServiceErrors where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeServiceErrors' request.
describeServiceErrors :: DescribeServiceErrors
describeServiceErrors = DescribeServiceErrors
    { _dserStackId = Nothing
    , _dserInstanceId = Nothing
    , _dserServiceErrorIds = mempty
    }

data DescribeServiceErrors = DescribeServiceErrors
    { _dserStackId :: Maybe Text
      -- ^ The stack ID. If you use this parameter, DescribeServiceErrors
      -- returns descriptions of the errors associated with the specified
      -- stack.
    , _dserInstanceId :: Maybe Text
      -- ^ The instance ID. If you use this parameter, DescribeServiceErrors
      -- returns descriptions of the errors associated with the specified
      -- instance.
    , _dserServiceErrorIds :: [Text]
      -- ^ An array of service error IDs. If you use this parameter,
      -- DescribeServiceErrors returns descriptions of the specified
      -- errors. Otherwise, it returns a description of every error.
    } deriving (Show, Generic)

makeLenses ''DescribeServiceErrors

instance ToPath DescribeServiceErrors

instance ToQuery DescribeServiceErrors

instance ToHeaders DescribeServiceErrors

instance ToJSON DescribeServiceErrors

data DescribeServiceErrorsResponse = DescribeServiceErrorsResponse
    { _dsesServiceErrors :: [ServiceError]
      -- ^ An array of ServiceError objects that describe the specified
      -- service errors.
    } deriving (Show, Generic)

makeLenses ''DescribeServiceErrorsResponse

instance FromJSON DescribeServiceErrorsResponse

instance AWSRequest DescribeServiceErrors where
    type Sv DescribeServiceErrors = OpsWorks
    type Rs DescribeServiceErrors = DescribeServiceErrorsResponse

    request = get
    response _ = jsonResponse
