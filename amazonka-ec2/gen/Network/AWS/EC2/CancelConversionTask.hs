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
-- Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.CancelConversionTask
    (
    -- * Request
      CancelConversion
    -- ** Request constructor
    , cancelConversionTask
    -- ** Request lenses
    , ccConversionTaskId
    , ccDryRun
    , ccReasonMessage

    -- * Response
    , CancelConversionTaskResponse
    -- ** Response constructor
    , cancelConversionTaskResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CancelConversion = CancelConversion
    { _ccConversionTaskId :: Text
    , _ccDryRun           :: Maybe Bool
    , _ccReasonMessage    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CancelConversion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccConversionTaskId' @::@ 'Text'
--
-- * 'ccDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ccReasonMessage' @::@ 'Maybe' 'Text'
--
cancelConversionTask :: Text -- ^ 'ccConversionTaskId'
                     -> CancelConversion
cancelConversionTask p1 = CancelConversion
    { _ccConversionTaskId = p1
    , _ccDryRun           = Nothing
    , _ccReasonMessage    = Nothing
    }

-- | The ID of the conversion task.
ccConversionTaskId :: Lens' CancelConversion Text
ccConversionTaskId =
    lens _ccConversionTaskId (\s a -> s { _ccConversionTaskId = a })

ccDryRun :: Lens' CancelConversion (Maybe Bool)
ccDryRun = lens _ccDryRun (\s a -> s { _ccDryRun = a })

ccReasonMessage :: Lens' CancelConversion (Maybe Text)
ccReasonMessage = lens _ccReasonMessage (\s a -> s { _ccReasonMessage = a })

instance ToQuery CancelConversion

instance ToPath CancelConversion where
    toPath = const "/"

data CancelConversionTaskResponse = CancelConversionTaskResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CancelConversionTaskResponse' constructor.
cancelConversionTaskResponse :: CancelConversionTaskResponse
cancelConversionTaskResponse = CancelConversionTaskResponse

instance FromXML CancelConversionTaskResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CancelConversionTaskResponse"

instance AWSRequest CancelConversion where
    type Sv CancelConversion = EC2
    type Rs CancelConversion = CancelConversionTaskResponse

    request  = post "CancelConversionTask"
    response = nullaryResponse CancelConversionTaskResponse
