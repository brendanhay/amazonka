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

-- Module      : Network.AWS.EC2.ConfirmProductInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Determines whether a product code is associated with an instance. This
-- action can only be used by the owner of the product code. It is useful when
-- a product code owner needs to verify whether another user's instance is
-- eligible for support.
module Network.AWS.EC2.ConfirmProductInstance
    (
    -- * Request
      ConfirmProductInstance
    -- ** Request constructor
    , confirmProductInstance
    -- ** Request lenses
    , cpiDryRun
    , cpiInstanceId
    , cpiProductCode

    -- * Response
    , ConfirmProductInstanceResult
    -- ** Response constructor
    , confirmProductInstanceResult
    -- ** Response lenses
    , cpirOwnerId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data ConfirmProductInstance = ConfirmProductInstance
    { _cpiDryRun      :: Maybe Bool
    , _cpiInstanceId  :: Text
    , _cpiProductCode :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ConfirmProductInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpiDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cpiInstanceId' @::@ 'Text'
--
-- * 'cpiProductCode' @::@ 'Text'
--
confirmProductInstance :: Text -- ^ 'cpiProductCode'
                       -> Text -- ^ 'cpiInstanceId'
                       -> ConfirmProductInstance
confirmProductInstance p1 p2 = ConfirmProductInstance
    { _cpiProductCode = p1
    , _cpiInstanceId  = p2
    , _cpiDryRun      = Nothing
    }

cpiDryRun :: Lens' ConfirmProductInstance (Maybe Bool)
cpiDryRun = lens _cpiDryRun (\s a -> s { _cpiDryRun = a })

-- | The ID of the instance.
cpiInstanceId :: Lens' ConfirmProductInstance Text
cpiInstanceId = lens _cpiInstanceId (\s a -> s { _cpiInstanceId = a })

-- | The product code. This must be a product code that you own.
cpiProductCode :: Lens' ConfirmProductInstance Text
cpiProductCode = lens _cpiProductCode (\s a -> s { _cpiProductCode = a })

instance ToPath ConfirmProductInstance where
    toPath = const "/"

instance ToQuery ConfirmProductInstance

newtype ConfirmProductInstanceResult = ConfirmProductInstanceResult
    { _cpirOwnerId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ConfirmProductInstanceResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpirOwnerId' @::@ 'Maybe' 'Text'
--
confirmProductInstanceResult :: ConfirmProductInstanceResult
confirmProductInstanceResult = ConfirmProductInstanceResult
    { _cpirOwnerId = Nothing
    }

-- | The AWS account ID of the instance owner. This is only present if the
-- product code is attached to the instance.
cpirOwnerId :: Lens' ConfirmProductInstanceResult (Maybe Text)
cpirOwnerId = lens _cpirOwnerId (\s a -> s { _cpirOwnerId = a })

instance AWSRequest ConfirmProductInstance where
    type Sv ConfirmProductInstance = EC2
    type Rs ConfirmProductInstance = ConfirmProductInstanceResult

    request  = post "ConfirmProductInstance"
    response = const . xmlResponse $ \h x -> ConfirmProductInstanceResult
newtype
