{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CancelConversionTask
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
module Network.AWS.EC2
    (
    -- * Request
      CancelConversionTask
    -- ** Request constructor
    , mkCancelConversionTask
    -- ** Request lenses
    , cctConversionTaskId
    , cctReasonMessage

    -- * Response
    , CancelConversionTaskResponse
    -- ** Response constructor
    , mkCancelConversionTaskResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data CancelConversionTask = CancelConversionTask
    { _cctConversionTaskId :: !Text
    , _cctReasonMessage :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CancelConversionTask' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ConversionTaskId ::@ @Text@
--
-- * @ReasonMessage ::@ @Maybe Text@
--
mkCancelConversionTask :: Text -- ^ 'cctConversionTaskId'
                       -> CancelConversionTask
mkCancelConversionTask p1 = CancelConversionTask
    { _cctConversionTaskId = p1
    , _cctReasonMessage = Nothing
    }

-- | The ID of the conversion task.
cctConversionTaskId :: Lens' CancelConversionTask Text
cctConversionTaskId =
    lens _cctConversionTaskId (\s a -> s { _cctConversionTaskId = a })

-- | 
cctReasonMessage :: Lens' CancelConversionTask (Maybe Text)
cctReasonMessage =
    lens _cctReasonMessage (\s a -> s { _cctReasonMessage = a })

instance ToQuery CancelConversionTask where
    toQuery = genericQuery def

data CancelConversionTaskResponse = CancelConversionTaskResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CancelConversionTaskResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCancelConversionTaskResponse :: CancelConversionTaskResponse
mkCancelConversionTaskResponse = CancelConversionTaskResponse

instance AWSRequest CancelConversionTask where
    type Sv CancelConversionTask = EC2
    type Rs CancelConversionTask = CancelConversionTaskResponse

    request = post "CancelConversionTask"
    response _ = nullaryResponse CancelConversionTaskResponse
