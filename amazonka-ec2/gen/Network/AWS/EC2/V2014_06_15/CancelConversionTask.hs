{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CancelConversionTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels an active conversion task. The task can be the import of an
-- instance or volume. The action removes all artifacts of the conversion,
-- including a partially uploaded volume or instance. If the conversion is
-- complete or is in the process of transferring the final disk image, the
-- command fails and returns an exception. For more information, see Using the
-- Command Line Tools to Import Your Virtual Machine to Amazon EC2 in the
-- Amazon Elastic Compute Cloud User Guide. Example This example request
-- cancels the conversion task with the ID import-i-fh95npoc.
-- https://ec2.amazonaws.com/?Action=CancelConversionTask
-- &amp;ConversionTaskId=import-i-fh95npoc &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE true.
module Network.AWS.EC2.V2014_06_15.CancelConversionTask
    (
    -- * Request
      CancelConversionTask
    -- ** Request constructor
    , cancelConversionTask
    -- ** Request lenses
    , ccrConversionTaskId
    , ccrReasonMessage

    -- * Response
    , CancelConversionTaskResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CancelConversionTask' request.
cancelConversionTask :: Text -- ^ 'ccrConversionTaskId'
                     -> CancelConversionTask
cancelConversionTask p1 = CancelConversionTask
    { _ccrConversionTaskId = p1
    , _ccrReasonMessage = Nothing
    }

data CancelConversionTask = CancelConversionTask
    { _ccrConversionTaskId :: Text
      -- ^ The ID of the conversion task.
    , _ccrReasonMessage :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

-- | The ID of the conversion task.
ccrConversionTaskId
    :: Functor f
    => (Text
    -> f (Text))
    -> CancelConversionTask
    -> f CancelConversionTask
ccrConversionTaskId f x =
    (\y -> x { _ccrConversionTaskId = y })
       <$> f (_ccrConversionTaskId x)
{-# INLINE ccrConversionTaskId #-}

-- | 
ccrReasonMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CancelConversionTask
    -> f CancelConversionTask
ccrReasonMessage f x =
    (\y -> x { _ccrReasonMessage = y })
       <$> f (_ccrReasonMessage x)
{-# INLINE ccrReasonMessage #-}

instance ToQuery CancelConversionTask where
    toQuery = genericQuery def

data CancelConversionTaskResponse = CancelConversionTaskResponse
    deriving (Eq, Show, Generic)

instance AWSRequest CancelConversionTask where
    type Sv CancelConversionTask = EC2
    type Rs CancelConversionTask = CancelConversionTaskResponse

    request = post "CancelConversionTask"
    response _ = nullaryResponse CancelConversionTaskResponse
