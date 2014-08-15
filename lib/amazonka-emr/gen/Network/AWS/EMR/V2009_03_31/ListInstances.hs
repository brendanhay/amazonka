{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.V2009_03_31.ListInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides information about the cluster instances that Amazon EMR provisions
-- on behalf of a user when it creates the cluster. For example, this
-- operation indicates when the EC2 instances reach the Ready state, when
-- instances become available to Amazon EMR to use for jobs, and the IP
-- addresses for cluster instances, etc.
module Network.AWS.EMR.V2009_03_31.ListInstances where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.EMR.V2009_03_31.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'ListInstances' request.
listInstances :: Text -- ^ '_liiClusterId'
              -> ListInstances
listInstances p1 = ListInstances
    { _liiClusterId = p1
    , _liiInstanceGroupId = Nothing
    , _liiInstanceGroupTypes = mempty
    , _liiMarker = Nothing
    }

data ListInstances = ListInstances
    { _liiClusterId :: Text
      -- ^ The identifier of the cluster for which to list the instances.
    , _liiInstanceGroupId :: Maybe Text
      -- ^ The identifier of the instance group for which to list the
      -- instances.
    , _liiInstanceGroupTypes :: [InstanceGroupType]
      -- ^ The type of instance group for which to list the instances.
    , _liiMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    } deriving (Show, Generic)

makeLenses ''ListInstances

instance ToPath ListInstances

instance ToQuery ListInstances

instance ToHeaders ListInstances

instance ToJSON ListInstances

data ListInstancesResponse = ListInstancesResponse
    { _lioInstances :: [Instance]
      -- ^ The list of instances for the cluster and given filters.
    , _lioMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    } deriving (Show, Generic)

makeLenses ''ListInstancesResponse

instance FromJSON ListInstancesResponse

instance AWSRequest ListInstances where
    type Sv ListInstances = EMR
    type Rs ListInstances = ListInstancesResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListInstances where
    next rq rs = (\x -> rq { _liiMarker = Just x })
        <$> (_lioMarker rs)
