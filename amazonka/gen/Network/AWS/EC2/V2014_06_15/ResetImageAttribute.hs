{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ResetImageAttribute
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
module Network.AWS.EC2.V2014_06_15.ResetImageAttribute where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'ResetImageAttribute' request.
resetImageAttribute :: ResetImageAttributeName -- ^ '_riasAttribute'
                    -> Text -- ^ '_riasImageId'
                    -> ResetImageAttribute
resetImageAttribute p1 p2 = ResetImageAttribute
    { _riasAttribute = p1
    , _riasImageId = p2
    , _riasDryRun = Nothing
    }

data ResetImageAttribute = ResetImageAttribute
    { _riasAttribute :: ResetImageAttributeName
      -- ^ The attribute to reset (currently you can only reset the launch
      -- permission attribute).
    , _riasImageId :: Text
      -- ^ The ID of the AMI.
    , _riasDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery ResetImageAttribute where
    toQuery = genericToQuery def

instance AWSRequest ResetImageAttribute where
    type Sv ResetImageAttribute = EC2
    type Rs ResetImageAttribute = ResetImageAttributeResponse

    request = post "ResetImageAttribute"
    response _ _ = return (Right ResetImageAttributeResponse)

data ResetImageAttributeResponse = ResetImageAttributeResponse
    deriving (Eq, Show, Generic)
