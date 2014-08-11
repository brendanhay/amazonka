{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteNetworkAclEntryResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteNetworkAclEntry where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteNetworkAclEntry' request.
deleteNetworkAclEntry :: Bool -- ^ '_dnaerEgress'
                      -> Integer -- ^ '_dnaerRuleNumber'
                      -> Text -- ^ '_dnaerNetworkAclId'
                      -> DeleteNetworkAclEntry
deleteNetworkAclEntry p1 p2 p3 = DeleteNetworkAclEntry
    { _dnaerEgress = p1
    , _dnaerRuleNumber = p2
    , _dnaerNetworkAclId = p3
    , _dnaerDryRun = Nothing
    }

data DeleteNetworkAclEntry = DeleteNetworkAclEntry
    { _dnaerEgress :: Bool
      -- ^ Indicates whether the rule is an egress rule.
    , _dnaerRuleNumber :: Integer
      -- ^ The rule number of the entry to delete.
    , _dnaerNetworkAclId :: Text
      -- ^ The ID of the network ACL.
    , _dnaerDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''DeleteNetworkAclEntry

instance ToQuery DeleteNetworkAclEntry where
    toQuery = genericToQuery def

data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteNetworkAclEntryResponse

instance AWSRequest DeleteNetworkAclEntry where
    type Sv DeleteNetworkAclEntry = EC2
    type Rs DeleteNetworkAclEntry = DeleteNetworkAclEntryResponse

    request = post "DeleteNetworkAclEntry"
    response _ = nullaryResponse DeleteNetworkAclEntryResponse
