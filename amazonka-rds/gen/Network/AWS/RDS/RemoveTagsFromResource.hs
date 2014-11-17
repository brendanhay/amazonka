{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.RemoveTagsFromResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes metadata tags from an Amazon RDS resource. For an overview on
-- tagging an Amazon RDS resource, see Tagging Amazon RDS Resources.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_RemoveTagsFromResource.html>
module Network.AWS.RDS.RemoveTagsFromResource
    (
    -- * Request
      RemoveTagsFromResource
    -- ** Request constructor
    , removeTagsFromResource
    -- ** Request lenses
    , rtfrResourceName
    , rtfrTagKeys

    -- * Response
    , RemoveTagsFromResourceResponse
    -- ** Response constructor
    , removeTagsFromResourceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data RemoveTagsFromResource = RemoveTagsFromResource
    { _rtfrResourceName :: Text
    , _rtfrTagKeys      :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'RemoveTagsFromResource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtfrResourceName' @::@ 'Text'
--
-- * 'rtfrTagKeys' @::@ ['Text']
--
removeTagsFromResource :: Text -- ^ 'rtfrResourceName'
                       -> RemoveTagsFromResource
removeTagsFromResource p1 = RemoveTagsFromResource
    { _rtfrResourceName = p1
    , _rtfrTagKeys      = mempty
    }

-- | The Amazon RDS resource the tags will be removed from. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- Constructing an RDS Amazon Resource Name (ARN).
rtfrResourceName :: Lens' RemoveTagsFromResource Text
rtfrResourceName = lens _rtfrResourceName (\s a -> s { _rtfrResourceName = a })

-- | The tag key (name) of the tag to be removed.
rtfrTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeys = lens _rtfrTagKeys (\s a -> s { _rtfrTagKeys = a })

data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RemoveTagsFromResourceResponse' constructor.
removeTagsFromResourceResponse :: RemoveTagsFromResourceResponse
removeTagsFromResourceResponse = RemoveTagsFromResourceResponse

instance AWSRequest RemoveTagsFromResource where
    type Sv RemoveTagsFromResource = RDS
    type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse

    request  = post "RemoveTagsFromResource"
    response = nullResponse RemoveTagsFromResourceResponse

instance ToPath RemoveTagsFromResource where
    toPath = const "/"

instance ToHeaders RemoveTagsFromResource

instance ToQuery RemoveTagsFromResource
