{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.RemoveTagsFromResource
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
module Network.AWS.RDS.V2013_09_09.RemoveTagsFromResource
    (
    -- * Request
      RemoveTagsFromResource
    -- ** Request constructor
    , mkRemoveTagsFromResource
    -- ** Request lenses
    , rtfrResourceName
    , rtfrTagKeys

    -- * Response
    , RemoveTagsFromResourceResponse
    -- ** Response constructor
    , mkRemoveTagsFromResourceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
data RemoveTagsFromResource = RemoveTagsFromResource
    { _rtfrResourceName :: Text
    , _rtfrTagKeys :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveTagsFromResource' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceName ::@ @Text@
--
-- * @TagKeys ::@ @[Text]@
--
mkRemoveTagsFromResource :: Text -- ^ 'rtfrResourceName'
                         -> [Text] -- ^ 'rtfrTagKeys'
                         -> RemoveTagsFromResource
mkRemoveTagsFromResource p1 p2 = RemoveTagsFromResource
    { _rtfrResourceName = p1
    , _rtfrTagKeys = p2
    }

-- | The Amazon RDS resource the tags will be removed from. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- Constructing an RDS Amazon Resource Name (ARN).
rtfrResourceName :: Lens' RemoveTagsFromResource Text
rtfrResourceName =
    lens _rtfrResourceName (\s a -> s { _rtfrResourceName = a })

-- | The tag key (name) of the tag to be removed.
rtfrTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeys = lens _rtfrTagKeys (\s a -> s { _rtfrTagKeys = a })

instance ToQuery RemoveTagsFromResource where
    toQuery = genericQuery def

data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveTagsFromResourceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkRemoveTagsFromResourceResponse :: RemoveTagsFromResourceResponse
mkRemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse

instance AWSRequest RemoveTagsFromResource where
    type Sv RemoveTagsFromResource = RDS
    type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse

    request = post "RemoveTagsFromResource"
    response _ = nullaryResponse RemoveTagsFromResourceResponse
