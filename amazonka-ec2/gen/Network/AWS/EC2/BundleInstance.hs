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

-- Module      : Network.AWS.EC2.BundleInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Bundles an Amazon instance store-backed Windows instance. During bundling,
-- only the root device volume (C:\) is bundled. Data on other instance store
-- volumes is not preserved. For more information, see Creating an Instance
-- Store-Backed Windows AMI.
module Network.AWS.EC2.BundleInstance
    (
    -- * Request
      BundleInstance
    -- ** Request constructor
    , bundleInstance
    -- ** Request lenses
    , biDryRun
    , biInstanceId
    , biStorage

    -- * Response
    , BundleInstanceResult
    -- ** Response constructor
    , bundleInstanceResult
    -- ** Response lenses
    , birBundleTask
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data BundleInstance = BundleInstance
    { _biDryRun     :: Maybe Bool
    , _biInstanceId :: Text
    , _biStorage    :: Storage
    } deriving (Eq, Show, Generic)

-- | 'BundleInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'biDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'biInstanceId' @::@ 'Text'
--
-- * 'biStorage' @::@ 'Storage'
--
bundleInstance :: Text -- ^ 'biInstanceId'
               -> Storage -- ^ 'biStorage'
               -> BundleInstance
bundleInstance p1 p2 = BundleInstance
    { _biInstanceId = p1
    , _biStorage    = p2
    , _biDryRun     = Nothing
    }

biDryRun :: Lens' BundleInstance (Maybe Bool)
biDryRun = lens _biDryRun (\s a -> s { _biDryRun = a })

-- | The ID of the instance to bundle.
biInstanceId :: Lens' BundleInstance Text
biInstanceId = lens _biInstanceId (\s a -> s { _biInstanceId = a })

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If
-- you specify a bucket that belongs to someone else, Amazon EC2 returns an
-- error.
biStorage :: Lens' BundleInstance Storage
biStorage = lens _biStorage (\s a -> s { _biStorage = a })
instance ToQuery BundleInstance

instance ToPath BundleInstance where
    toPath = const "/"

newtype BundleInstanceResult = BundleInstanceResult
    { _birBundleTask :: Maybe BundleTask
    } deriving (Eq, Show, Generic)

-- | 'BundleInstanceResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'birBundleTask' @::@ 'Maybe' 'BundleTask'
--
bundleInstanceResult :: BundleInstanceResult
bundleInstanceResult = BundleInstanceResult
    { _birBundleTask = Nothing
    }

-- | Information about the bundle task.
birBundleTask :: Lens' BundleInstanceResult (Maybe BundleTask)
birBundleTask = lens _birBundleTask (\s a -> s { _birBundleTask = a })
instance FromXML BundleInstanceResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BundleInstanceResult"

instance AWSRequest BundleInstance where
    type Sv BundleInstance = EC2
    type Rs BundleInstance = BundleInstanceResult

    request  = post "BundleInstance"
    response = xmlResponse $ \h x -> BundleInstanceResult
        <$> x %| "bundleInstanceTask"
