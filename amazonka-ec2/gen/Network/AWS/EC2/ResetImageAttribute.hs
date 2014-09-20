{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Resets an attribute of an AMI to its default value. Example This example
-- resets the launchPermission attribute for the specified AMI.
-- https://ec2.amazonaws.com/?Action=ResetImageAttribute
-- &amp;ImageId=ami-61a54008 &amp;Attribute=launchPermission &amp;AUTHPARAMS
-- &lt;ResetImageAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ResetImageAttributeResponse&gt;.
module Network.AWS.EC2.ResetImageAttribute
    (
    -- * Request
      ResetImageAttribute
    -- ** Request constructor
    , resetImageAttribute
    -- ** Request lenses
    , riaImageId
    , riaAttribute

    -- * Response
    , ResetImageAttributeResponse
    -- ** Response constructor
    , resetImageAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ResetImageAttribute = ResetImageAttribute
    { _riaImageId :: Text
    , _riaAttribute :: ResetImageAttributeName
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetImageAttribute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ImageId ::@ @Text@
--
-- * @Attribute ::@ @ResetImageAttributeName@
--
resetImageAttribute :: Text -- ^ 'riaImageId'
                    -> ResetImageAttributeName -- ^ 'riaAttribute'
                    -> ResetImageAttribute
resetImageAttribute p1 p2 = ResetImageAttribute
    { _riaImageId = p1
    , _riaAttribute = p2
    }

-- | The ID of the AMI.
riaImageId :: Lens' ResetImageAttribute Text
riaImageId = lens _riaImageId (\s a -> s { _riaImageId = a })

-- | The attribute to reset (currently you can only reset the launch permission
-- attribute).
riaAttribute :: Lens' ResetImageAttribute ResetImageAttributeName
riaAttribute = lens _riaAttribute (\s a -> s { _riaAttribute = a })

instance ToQuery ResetImageAttribute where
    toQuery = genericQuery def

data ResetImageAttributeResponse = ResetImageAttributeResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetImageAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
resetImageAttributeResponse :: ResetImageAttributeResponse
resetImageAttributeResponse = ResetImageAttributeResponse

instance AWSRequest ResetImageAttribute where
    type Sv ResetImageAttribute = EC2
    type Rs ResetImageAttribute = ResetImageAttributeResponse

    request = post "ResetImageAttribute"
    response _ = nullaryResponse ResetImageAttributeResponse
