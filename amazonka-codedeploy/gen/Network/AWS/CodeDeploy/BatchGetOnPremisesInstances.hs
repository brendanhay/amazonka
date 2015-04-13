{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets information about one or more on-premises instances.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_BatchGetOnPremisesInstances.html>
module Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
    (
    -- * Request
      BatchGetOnPremisesInstances
    -- ** Request constructor
    , batchGetOnPremisesInstances
    -- ** Request lenses
    , bgopiInstanceNames

    -- * Response
    , BatchGetOnPremisesInstancesResponse
    -- ** Response constructor
    , batchGetOnPremisesInstancesResponse
    -- ** Response lenses
    , bgopirInstanceInfos
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype BatchGetOnPremisesInstances = BatchGetOnPremisesInstances
    { _bgopiInstanceNames :: List "instanceNames" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList BatchGetOnPremisesInstances where
    type Item BatchGetOnPremisesInstances = Text

    fromList = BatchGetOnPremisesInstances . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _bgopiInstanceNames

-- | 'BatchGetOnPremisesInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgopiInstanceNames' @::@ ['Text']
--
batchGetOnPremisesInstances :: BatchGetOnPremisesInstances
batchGetOnPremisesInstances = BatchGetOnPremisesInstances
    { _bgopiInstanceNames = mempty
    }

-- | The names of the on-premises instances to get information about.
bgopiInstanceNames :: Lens' BatchGetOnPremisesInstances [Text]
bgopiInstanceNames =
    lens _bgopiInstanceNames (\s a -> s { _bgopiInstanceNames = a })
        . _List

newtype BatchGetOnPremisesInstancesResponse = BatchGetOnPremisesInstancesResponse
    { _bgopirInstanceInfos :: List "instanceInfos" InstanceInfo
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList BatchGetOnPremisesInstancesResponse where
    type Item BatchGetOnPremisesInstancesResponse = InstanceInfo

    fromList = BatchGetOnPremisesInstancesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _bgopirInstanceInfos

-- | 'BatchGetOnPremisesInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgopirInstanceInfos' @::@ ['InstanceInfo']
--
batchGetOnPremisesInstancesResponse :: BatchGetOnPremisesInstancesResponse
batchGetOnPremisesInstancesResponse = BatchGetOnPremisesInstancesResponse
    { _bgopirInstanceInfos = mempty
    }

-- | Information about the on-premises instances.
bgopirInstanceInfos :: Lens' BatchGetOnPremisesInstancesResponse [InstanceInfo]
bgopirInstanceInfos =
    lens _bgopirInstanceInfos (\s a -> s { _bgopirInstanceInfos = a })
        . _List

instance ToPath BatchGetOnPremisesInstances where
    toPath = const "/"

instance ToQuery BatchGetOnPremisesInstances where
    toQuery = const mempty

instance ToHeaders BatchGetOnPremisesInstances

instance ToJSON BatchGetOnPremisesInstances where
    toJSON BatchGetOnPremisesInstances{..} = object
        [ "instanceNames" .= _bgopiInstanceNames
        ]

instance AWSRequest BatchGetOnPremisesInstances where
    type Sv BatchGetOnPremisesInstances = CodeDeploy
    type Rs BatchGetOnPremisesInstances = BatchGetOnPremisesInstancesResponse

    request  = post "BatchGetOnPremisesInstances"
    response = jsonResponse

instance FromJSON BatchGetOnPremisesInstancesResponse where
    parseJSON = withObject "BatchGetOnPremisesInstancesResponse" $ \o -> BatchGetOnPremisesInstancesResponse
        <$> o .:? "instanceInfos" .!= mempty
