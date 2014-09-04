{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ResetInstanceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Resets an attribute of an instance to its default value. To reset the
-- kernel or RAM disk, the instance must be in a stopped state. To reset the
-- SourceDestCheck, the instance can be either running or stopped. The
-- SourceDestCheck attribute controls whether source/destination checking is
-- enabled. The default value is true, which means checking is enabled. This
-- value must be false for a NAT instance to perform NAT. For more
-- information, see NAT Instances in the Amazon Virtual Private Cloud User
-- Guide. Example This example resets the sourceDestCheck attribute.
-- https://ec2.amazonaws.com/?Action=ResetInstanceAttribute
-- &amp;InstanceId=i-1a2b3c4d &amp;Attribute=sourceDestCheck &amp;AUTHPARAMS
-- &lt;ResetInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ResetInstanceAttributeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.ResetInstanceAttribute
    (
    -- * Request
      ResetInstanceAttribute
    -- ** Request constructor
    , mkResetInstanceAttributeRequest
    -- ** Request lenses
    , riasInstanceId
    , riasAttribute

    -- * Response
    , ResetInstanceAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetInstanceAttribute' request.
mkResetInstanceAttributeRequest :: Text -- ^ 'riasInstanceId'
                                -> InstanceAttributeName -- ^ 'riasAttribute'
                                -> ResetInstanceAttribute
mkResetInstanceAttributeRequest p1 p2 = ResetInstanceAttribute
    { _riasInstanceId = p1
    , _riasAttribute = p2
    }
{-# INLINE mkResetInstanceAttributeRequest #-}

data ResetInstanceAttribute = ResetInstanceAttribute
    { _riasInstanceId :: Text
      -- ^ The ID of the instance.
    , _riasAttribute :: InstanceAttributeName
      -- ^ The attribute to reset.
    } deriving (Show, Generic)

-- | The ID of the instance.
riasInstanceId :: Lens' ResetInstanceAttribute (Text)
riasInstanceId = lens _riasInstanceId (\s a -> s { _riasInstanceId = a })
{-# INLINE riasInstanceId #-}

-- | The attribute to reset.
riasAttribute :: Lens' ResetInstanceAttribute (InstanceAttributeName)
riasAttribute = lens _riasAttribute (\s a -> s { _riasAttribute = a })
{-# INLINE riasAttribute #-}

instance ToQuery ResetInstanceAttribute where
    toQuery = genericQuery def

data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ResetInstanceAttribute where
    type Sv ResetInstanceAttribute = EC2
    type Rs ResetInstanceAttribute = ResetInstanceAttributeResponse

    request = post "ResetInstanceAttribute"
    response _ = nullaryResponse ResetInstanceAttributeResponse
