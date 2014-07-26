{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.ModifyImageAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the specified attribute of the specified AMI. You can specify only
-- one attribute at a time. AWS Marketplace product codes cannot be modified.
-- Images with an AWS Marketplace product code cannot be made public. Example.
module Network.AWS.EC2.V2014_05_01.ModifyImageAttribute where

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
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data ModifyImageAttribute = ModifyImageAttribute
    { _miarImageId :: Text
      -- ^ The ID of the AMI.
    , _miarDescription :: AttributeValue
      -- ^ A description for the AMI.
    , _miarDryRun :: Bool
      -- ^ 
    , _miarLaunchPermission :: LaunchPermissionModifications
      -- ^ 
    , _miarProductCodes :: [Text]
      -- ^ One or more product codes. After you add a product code to an
      -- AMI, it can't be removed. This is only valid when modifying the
      -- productCodes attribute.
    , _miarAttribute :: Text
      -- ^ The name of the attribute to modify.
    , _miarValue :: Text
      -- ^ The value of the attribute being modified. This is only valid
      -- when modifying the description attribute.
    , _miarOperationType :: Text
      -- ^ The operation type.
    , _miarUserGroups :: [Text]
      -- ^ One or more user groups. This is only valid when modifying the
      -- launchPermission attribute.
    , _miarUserIds :: [Text]
      -- ^ One or more AWS account IDs. This is only valid when modifying
      -- the launchPermission attribute.
    } deriving (Generic)

instance ToQuery ModifyImageAttribute where
    toQuery = genericToQuery def

instance AWSRequest ModifyImageAttribute where
    type Sv ModifyImageAttribute = EC2
    type Rs ModifyImageAttribute = ModifyImageAttributeResponse

    request = post "ModifyImageAttribute"

    response _ _ = return (Right ModifyImageAttributeResponse)

data ModifyImageAttributeResponse = ModifyImageAttributeResponse
    deriving (Eq, Show, Generic)
