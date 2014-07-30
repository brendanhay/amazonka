{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DeleteOptionGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an existing option group. https://rds.amazonaws.com/
-- ?Action=DeleteOptionGroup &OptionGroupName=myoptiongroup.
module Network.AWS.RDS.V2013_09_09.DeleteOptionGroup where

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
import           Network.AWS.RDS.V2013_09_09.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteOptionGroup = DeleteOptionGroup
    { _dognOptionGroupName :: Text
      -- ^ The name of the option group to be deleted. You cannot delete
      -- default option groups.
    } deriving (Generic)

instance ToQuery DeleteOptionGroup where
    toQuery = genericToQuery def

instance AWSRequest DeleteOptionGroup where
    type Sv DeleteOptionGroup = RDS
    type Rs DeleteOptionGroup = DeleteOptionGroupResponse

    request = post "DeleteOptionGroup"
    response _ _ = return (Right DeleteOptionGroupResponse)

data DeleteOptionGroupResponse = DeleteOptionGroupResponse
    deriving (Eq, Show, Generic)
