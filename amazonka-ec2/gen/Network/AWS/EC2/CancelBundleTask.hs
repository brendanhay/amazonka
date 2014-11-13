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

-- Module      : Network.AWS.EC2.CancelBundleTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels a bundling operation for an instance store-backed Windows instance.
module Network.AWS.EC2.CancelBundleTask
    (
    -- * Request
      CancelBundleTask
    -- ** Request constructor
    , cancelBundleTask
    -- ** Request lenses
    , cbtBundleId
    , cbtDryRun

    -- * Response
    , CancelBundleTaskResponse
    -- ** Response constructor
    , cancelBundleTaskResponse
    -- ** Response lenses
    , cbtrBundleTask
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CancelBundleTask = CancelBundleTask
    { _cbtBundleId :: Text
    , _cbtDryRun   :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'CancelBundleTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbtBundleId' @::@ 'Text'
--
-- * 'cbtDryRun' @::@ 'Maybe' 'Bool'
--
cancelBundleTask :: Text -- ^ 'cbtBundleId'
                 -> CancelBundleTask
cancelBundleTask p1 = CancelBundleTask
    { _cbtBundleId = p1
    , _cbtDryRun   = Nothing
    }

-- | The ID of the bundle task.
cbtBundleId :: Lens' CancelBundleTask Text
cbtBundleId = lens _cbtBundleId (\s a -> s { _cbtBundleId = a })

cbtDryRun :: Lens' CancelBundleTask (Maybe Bool)
cbtDryRun = lens _cbtDryRun (\s a -> s { _cbtDryRun = a })

instance ToQuery CancelBundleTask

instance ToPath CancelBundleTask where
    toPath = const "/"

newtype CancelBundleTaskResponse = CancelBundleTaskResponse
    { _cbtrBundleTask :: Maybe BundleTask
    } deriving (Eq, Show, Generic)

-- | 'CancelBundleTaskResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbtrBundleTask' @::@ 'Maybe' 'BundleTask'
--
cancelBundleTaskResponse :: CancelBundleTaskResponse
cancelBundleTaskResponse = CancelBundleTaskResponse
    { _cbtrBundleTask = Nothing
    }

-- | The bundle task.
cbtrBundleTask :: Lens' CancelBundleTaskResponse (Maybe BundleTask)
cbtrBundleTask = lens _cbtrBundleTask (\s a -> s { _cbtrBundleTask = a })

instance AWSRequest CancelBundleTask where
    type Sv CancelBundleTask = EC2
    type Rs CancelBundleTask = CancelBundleTaskResponse

    request  = post "CancelBundleTask"
    response = xmlResponse $ \h x -> CancelBundleTaskResponse
        <$> x %| "bundleInstanceTask"
