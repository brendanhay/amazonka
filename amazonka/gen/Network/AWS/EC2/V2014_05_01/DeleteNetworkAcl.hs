{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DeleteNetworkAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified network ACL. You can't delete the ACL if it's
-- associated with any subnets. You can't delete the default network ACL.
-- Example This example deletes the specified network ACL.
-- https://ec2.amazonaws.com/?Action=DeleteNetworkAcl
-- &amp;NetworkAclId=acl-2cb85d45 &amp;AUTHPARAMS &lt;DeleteNetworkAclResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteNetworkAclResponse&gt;.
module Network.AWS.EC2.V2014_05_01.DeleteNetworkAcl where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteNetworkAcl = DeleteNetworkAcl
    { _dnarNetworkAclId :: Text
      -- ^ The ID of the network ACL.
    , _dnarDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery DeleteNetworkAcl where
    toQuery = genericToQuery def

instance AWSRequest DeleteNetworkAcl where
    type Sv DeleteNetworkAcl = EC2
    type Rs DeleteNetworkAcl = DeleteNetworkAclResponse

    request = post "DeleteNetworkAcl"
    response _ _ = return (Right DeleteNetworkAclResponse)

data DeleteNetworkAclResponse = DeleteNetworkAclResponse
    deriving (Eq, Show, Generic)
