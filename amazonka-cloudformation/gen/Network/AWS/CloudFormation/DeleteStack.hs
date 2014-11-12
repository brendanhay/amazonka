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

-- Module      : Network.AWS.CloudFormation.DeleteStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified stack. Once the call completes successfully, stack
-- deletion starts. Deleted stacks do not show up in the DescribeStacks API if
-- the deletion has been completed successfully.
module Network.AWS.CloudFormation.DeleteStack
    (
    -- * Request
      DeleteStackInput
    -- ** Request constructor
    , deleteStackInput
    -- ** Request lenses
    , dsi1StackName

    -- * Response
    , DeleteStackResponse
    -- ** Response constructor
    , deleteStackResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

newtype DeleteStackInput = DeleteStackInput
    { _dsi1StackName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteStackInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsi1StackName' @::@ 'Text'
--
deleteStackInput :: Text -- ^ 'dsi1StackName'
                 -> DeleteStackInput
deleteStackInput p1 = DeleteStackInput
    { _dsi1StackName = p1
    }

-- | The name or the unique identifier associated with the stack.
dsi1StackName :: Lens' DeleteStackInput Text
dsi1StackName = lens _dsi1StackName (\s a -> s { _dsi1StackName = a })

instance ToQuery DeleteStackInput

instance ToPath DeleteStackInput where
    toPath = const "/"

data DeleteStackResponse = DeleteStackResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteStackResponse' constructor.
deleteStackResponse :: DeleteStackResponse
deleteStackResponse = DeleteStackResponse

instance FromXML DeleteStackResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteStackResponse"

instance AWSRequest DeleteStackInput where
    type Sv DeleteStackInput = CloudFormation
    type Rs DeleteStackInput = DeleteStackResponse

    request  = post "DeleteStack"
    response = nullaryResponse DeleteStackResponse
