{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ReplaceNetworkAclAssociation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes which network ACL a subnet is associated with. By default when you
-- create a subnet, it's automatically associated with the default network
-- ACL. For more information about network ACLs, see Network ACLs in the
-- Amazon Virtual Private Cloud User Guide. Example This example starts with a
-- network ACL associated with a subnet, and a corresponding association ID
-- aclassoc-e5b95c8c. You want to associate a different network ACL
-- (acl-5fb85d36) with the subnet. The result is a new association ID
-- representing the new association.
-- https://ec2.amazonaws.com/?Action=ReplaceNetworkAclAssociation
-- &amp;AssociationId=aclassoc-e5b95c8c &amp;NetworkAclId=acl-5fb85d36
-- &amp;AUTHPARAMS &lt;ReplaceNetworkAclAssociationResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;newAssociationId&gt;aclassoc-17b85d7e&lt;/newAssociationId&gt;
-- &lt;/ReplaceNetworkAclAssociationResponse&gt;.
module Network.AWS.EC2.V2014_06_15.ReplaceNetworkAclAssociation
    (
    -- * Request
      ReplaceNetworkAclAssociation
    -- ** Request constructor
    , replaceNetworkAclAssociation
    -- ** Request lenses
    , rnaarAssociationId
    , rnaarNetworkAclId

    -- * Response
    , ReplaceNetworkAclAssociationResponse
    -- ** Response lenses
    , rnaasNewAssociationId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ReplaceNetworkAclAssociation' request.
replaceNetworkAclAssociation :: Text -- ^ 'rnaarAssociationId'
                             -> Text -- ^ 'rnaarNetworkAclId'
                             -> ReplaceNetworkAclAssociation
replaceNetworkAclAssociation p1 p2 = ReplaceNetworkAclAssociation
    { _rnaarAssociationId = p1
    , _rnaarNetworkAclId = p2
    }
{-# INLINE replaceNetworkAclAssociation #-}

data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation
    { _rnaarAssociationId :: Text
      -- ^ The ID of the current association between the original network
      -- ACL and the subnet.
    , _rnaarNetworkAclId :: Text
      -- ^ The ID of the new ACL to associate with the subnet.
    } deriving (Show, Generic)

-- | The ID of the current association between the original network ACL and the
-- subnet.
rnaarAssociationId :: Lens' ReplaceNetworkAclAssociation (Text)
rnaarAssociationId f x =
    f (_rnaarAssociationId x)
        <&> \y -> x { _rnaarAssociationId = y }
{-# INLINE rnaarAssociationId #-}

-- | The ID of the new ACL to associate with the subnet.
rnaarNetworkAclId :: Lens' ReplaceNetworkAclAssociation (Text)
rnaarNetworkAclId f x =
    f (_rnaarNetworkAclId x)
        <&> \y -> x { _rnaarNetworkAclId = y }
{-# INLINE rnaarNetworkAclId #-}

instance ToQuery ReplaceNetworkAclAssociation where
    toQuery = genericQuery def

data ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
    { _rnaasNewAssociationId :: Maybe Text
      -- ^ The ID of the new association.
    } deriving (Show, Generic)

-- | The ID of the new association.
rnaasNewAssociationId :: Lens' ReplaceNetworkAclAssociationResponse (Maybe Text)
rnaasNewAssociationId f x =
    f (_rnaasNewAssociationId x)
        <&> \y -> x { _rnaasNewAssociationId = y }
{-# INLINE rnaasNewAssociationId #-}

instance FromXML ReplaceNetworkAclAssociationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ReplaceNetworkAclAssociation where
    type Sv ReplaceNetworkAclAssociation = EC2
    type Rs ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociationResponse

    request = post "ReplaceNetworkAclAssociation"
    response _ = xmlResponse
