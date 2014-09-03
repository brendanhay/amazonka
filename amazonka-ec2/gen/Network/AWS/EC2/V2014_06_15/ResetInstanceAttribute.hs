{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.EC2.V2014_06_15.ResetInstanceAttribute where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data ResetInstanceAttribute = ResetInstanceAttribute
    { _riasAttribute :: InstanceAttributeName
      -- ^ The attribute to reset.
    , _riasInstanceId :: Text
      -- ^ The ID of the instance.
    } deriving (Show, Generic)

makeLenses ''ResetInstanceAttribute

instance ToQuery ResetInstanceAttribute where
    toQuery = genericQuery def

data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse
    deriving (Eq, Show, Generic)

makeLenses ''ResetInstanceAttributeResponse

instance AWSRequest ResetInstanceAttribute where
    type Sv ResetInstanceAttribute = EC2
    type Rs ResetInstanceAttribute = ResetInstanceAttributeResponse

    request = post "ResetInstanceAttribute"
    response _ = nullaryResponse ResetInstanceAttributeResponse
