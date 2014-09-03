{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteNetworkAclEntry
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified ingress or egress entry (rule) from the specified
-- network ACL. Example This example deletes ingress rule number 100 from the
-- specified network ACL.
-- https://ec2.amazonaws.com/?Action=DeleteNetworkAclEntry
-- &amp;NetworkAclId=acl-2cb85d45 &amp;RuleNumber=100 &amp;AUTHPARAMS
-- &lt;DeleteNetworkAclEntryResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteNetworkAclEntryResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteNetworkAclEntry
    (
    -- * Request
      DeleteNetworkAclEntry
    -- ** Request constructor
    , deleteNetworkAclEntry
    -- ** Request lenses
    , dnaerEgress
    , dnaerRuleNumber
    , dnaerNetworkAclId

    -- * Response
    , DeleteNetworkAclEntryResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteNetworkAclEntry' request.
deleteNetworkAclEntry :: Bool -- ^ 'dnaerEgress'
                      -> Integer -- ^ 'dnaerRuleNumber'
                      -> Text -- ^ 'dnaerNetworkAclId'
                      -> DeleteNetworkAclEntry
deleteNetworkAclEntry p1 p2 p3 = DeleteNetworkAclEntry
    { _dnaerEgress = p1
    , _dnaerRuleNumber = p2
    , _dnaerNetworkAclId = p3
    }

data DeleteNetworkAclEntry = DeleteNetworkAclEntry
    { _dnaerEgress :: Bool
      -- ^ Indicates whether the rule is an egress rule.
    , _dnaerRuleNumber :: Integer
      -- ^ The rule number of the entry to delete.
    , _dnaerNetworkAclId :: Text
      -- ^ The ID of the network ACL.
    } deriving (Show, Generic)

-- | Indicates whether the rule is an egress rule.
dnaerEgress
    :: Functor f
    => (Bool
    -> f (Bool))
    -> DeleteNetworkAclEntry
    -> f DeleteNetworkAclEntry
dnaerEgress f x =
    (\y -> x { _dnaerEgress = y })
       <$> f (_dnaerEgress x)
{-# INLINE dnaerEgress #-}

-- | The rule number of the entry to delete.
dnaerRuleNumber
    :: Functor f
    => (Integer
    -> f (Integer))
    -> DeleteNetworkAclEntry
    -> f DeleteNetworkAclEntry
dnaerRuleNumber f x =
    (\y -> x { _dnaerRuleNumber = y })
       <$> f (_dnaerRuleNumber x)
{-# INLINE dnaerRuleNumber #-}

-- | The ID of the network ACL.
dnaerNetworkAclId
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteNetworkAclEntry
    -> f DeleteNetworkAclEntry
dnaerNetworkAclId f x =
    (\y -> x { _dnaerNetworkAclId = y })
       <$> f (_dnaerNetworkAclId x)
{-# INLINE dnaerNetworkAclId #-}

instance ToQuery DeleteNetworkAclEntry where
    toQuery = genericQuery def

data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteNetworkAclEntry where
    type Sv DeleteNetworkAclEntry = EC2
    type Rs DeleteNetworkAclEntry = DeleteNetworkAclEntryResponse

    request = post "DeleteNetworkAclEntry"
    response _ = nullaryResponse DeleteNetworkAclEntryResponse
