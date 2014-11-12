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

-- Module      : Network.AWS.CloudFormation.CancelUpdateStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels an update on the specified stack. If the call completes
-- successfully, the stack will roll back the update and revert to the
-- previous stack configuration.
module Network.AWS.CloudFormation.CancelUpdateStack
    (
    -- * Request
      CancelUpdateStackInput
    -- ** Request constructor
    , cancelUpdateStackInput
    -- ** Request lenses
    , cusiStackName

    -- * Response
    , CancelUpdateStackResponse
    -- ** Response constructor
    , cancelUpdateStackResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

newtype CancelUpdateStackInput = CancelUpdateStackInput
    { _cusiStackName :: Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'CancelUpdateStackInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cusiStackName' @::@ 'Text'
--
cancelUpdateStackInput :: Text -- ^ 'cusiStackName'
                       -> CancelUpdateStackInput
cancelUpdateStackInput p1 = CancelUpdateStackInput
    { _cusiStackName = p1
    }

-- | The name or the unique identifier associated with the stack.
cusiStackName :: Lens' CancelUpdateStackInput Text
cusiStackName = lens _cusiStackName (\s a -> s { _cusiStackName = a })
instance ToQuery CancelUpdateStackInput

instance ToPath CancelUpdateStackInput where
    toPath = const "/"

data CancelUpdateStackResponse = CancelUpdateStackResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CancelUpdateStackResponse' constructor.
cancelUpdateStackResponse :: CancelUpdateStackResponse
cancelUpdateStackResponse = CancelUpdateStackResponse

instance FromXML CancelUpdateStackResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CancelUpdateStackResponse"

instance AWSRequest CancelUpdateStackInput where
    type Sv CancelUpdateStackInput = CloudFormation
    type Rs CancelUpdateStackInput = CancelUpdateStackResponse

    request  = post "CancelUpdateStack"
    response = nullaryResponse CancelUpdateStackResponse
