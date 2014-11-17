{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.BatchGetApplications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets information about one or more applications.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_BatchGetApplications.html>
module Network.AWS.CodeDeploy.BatchGetApplications
    (
    -- * Request
      BatchGetApplications
    -- ** Request constructor
    , batchGetApplications
    -- ** Request lenses
    , bgaApplicationNames

    -- * Response
    , BatchGetApplicationsResponse
    -- ** Response constructor
    , batchGetApplicationsResponse
    -- ** Response lenses
    , bgarApplicationsInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype BatchGetApplications = BatchGetApplications
    { _bgaApplicationNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList BatchGetApplications where
    type Item BatchGetApplications = Text

    fromList = BatchGetApplications . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _bgaApplicationNames

-- | 'BatchGetApplications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgaApplicationNames' @::@ ['Text']
--
batchGetApplications :: BatchGetApplications
batchGetApplications = BatchGetApplications
    { _bgaApplicationNames = mempty
    }

-- | A list of application names, with multiple application names separated by
-- spaces.
bgaApplicationNames :: Lens' BatchGetApplications [Text]
bgaApplicationNames =
    lens _bgaApplicationNames (\s a -> s { _bgaApplicationNames = a })

newtype BatchGetApplicationsResponse = BatchGetApplicationsResponse
    { _bgarApplicationsInfo :: [ApplicationInfo]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList BatchGetApplicationsResponse where
    type Item BatchGetApplicationsResponse = ApplicationInfo

    fromList = BatchGetApplicationsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _bgarApplicationsInfo

-- | 'BatchGetApplicationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgarApplicationsInfo' @::@ ['ApplicationInfo']
--
batchGetApplicationsResponse :: BatchGetApplicationsResponse
batchGetApplicationsResponse = BatchGetApplicationsResponse
    { _bgarApplicationsInfo = mempty
    }

-- | Information about the applications.
bgarApplicationsInfo :: Lens' BatchGetApplicationsResponse [ApplicationInfo]
bgarApplicationsInfo =
    lens _bgarApplicationsInfo (\s a -> s { _bgarApplicationsInfo = a })

instance ToPath BatchGetApplications where
    toPath = const "/"

instance ToQuery BatchGetApplications where
    toQuery = const mempty

instance ToHeaders BatchGetApplications

instance ToJSON BatchGetApplications where
    toJSON BatchGetApplications{..} = object
        [ "applicationNames" .= _bgaApplicationNames
        ]

instance AWSRequest BatchGetApplications where
    type Sv BatchGetApplications = CodeDeploy
    type Rs BatchGetApplications = BatchGetApplicationsResponse

    request  = post "BatchGetApplications"
    response = jsonResponse

instance FromJSON BatchGetApplicationsResponse where
    parseJSON = withObject "BatchGetApplicationsResponse" $ \o -> BatchGetApplicationsResponse
        <$> o .: "applicationsInfo"
