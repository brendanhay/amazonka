{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
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
module Network.AWS.EC2
    (
    -- * Request
      DeleteNetworkAclEntry
    -- ** Request constructor
    , mkDeleteNetworkAclEntry
    -- ** Request lenses
    , dnaeNetworkAclId
    , dnaeRuleNumber
    , dnaeEgress

    -- * Response
    , DeleteNetworkAclEntryResponse
    -- ** Response constructor
    , mkDeleteNetworkAclEntryResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DeleteNetworkAclEntry = DeleteNetworkAclEntry
    { _dnaeNetworkAclId :: !Text
    , _dnaeRuleNumber :: !Integer
    , _dnaeEgress :: !Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteNetworkAclEntry' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkAclId ::@ @Text@
--
-- * @RuleNumber ::@ @Integer@
--
-- * @Egress ::@ @Bool@
--
mkDeleteNetworkAclEntry :: Text -- ^ 'dnaeNetworkAclId'
                        -> Integer -- ^ 'dnaeRuleNumber'
                        -> Bool -- ^ 'dnaeEgress'
                        -> DeleteNetworkAclEntry
mkDeleteNetworkAclEntry p1 p2 p3 = DeleteNetworkAclEntry
    { _dnaeNetworkAclId = p1
    , _dnaeRuleNumber = p2
    , _dnaeEgress = p3
    }

-- | The ID of the network ACL.
dnaeNetworkAclId :: Lens' DeleteNetworkAclEntry Text
dnaeNetworkAclId =
    lens _dnaeNetworkAclId (\s a -> s { _dnaeNetworkAclId = a })

-- | The rule number of the entry to delete.
dnaeRuleNumber :: Lens' DeleteNetworkAclEntry Integer
dnaeRuleNumber = lens _dnaeRuleNumber (\s a -> s { _dnaeRuleNumber = a })

-- | Indicates whether the rule is an egress rule.
dnaeEgress :: Lens' DeleteNetworkAclEntry Bool
dnaeEgress = lens _dnaeEgress (\s a -> s { _dnaeEgress = a })

instance ToQuery DeleteNetworkAclEntry where
    toQuery = genericQuery def

data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteNetworkAclEntryResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteNetworkAclEntryResponse :: DeleteNetworkAclEntryResponse
mkDeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse

instance AWSRequest DeleteNetworkAclEntry where
    type Sv DeleteNetworkAclEntry = EC2
    type Rs DeleteNetworkAclEntry = DeleteNetworkAclEntryResponse

    request = post "DeleteNetworkAclEntry"
    response _ = nullaryResponse DeleteNetworkAclEntryResponse
