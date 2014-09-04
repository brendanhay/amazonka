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
    , mkRemoveTagsFromResourceMessage
    -- ** Request lenses
    , rtfrmResourceName
    , rtfrmTagKeys

    -- * Response
    , RemoveTagsFromResourceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveTagsFromResource' request.
mkRemoveTagsFromResourceMessage :: Text -- ^ 'rtfrmResourceName'
                                -> [Text] -- ^ 'rtfrmTagKeys'
                                -> RemoveTagsFromResource
mkRemoveTagsFromResourceMessage p1 p2 = RemoveTagsFromResource
    { _rtfrmResourceName = p1
    , _rtfrmTagKeys = p2
    }
{-# INLINE mkRemoveTagsFromResourceMessage #-}

data RemoveTagsFromResource = RemoveTagsFromResource
    { _rtfrmResourceName :: Text
      -- ^ The Amazon RDS resource the tags will be removed from. This value
      -- is an Amazon Resource Name (ARN). For information about creating
      -- an ARN, see Constructing an RDS Amazon Resource Name (ARN).
    , _rtfrmTagKeys :: [Text]
      -- ^ The tag key (name) of the tag to be removed.
    } deriving (Show, Generic)

-- | The Amazon RDS resource the tags will be removed from. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- Constructing an RDS Amazon Resource Name (ARN).
rtfrmResourceName :: Lens' RemoveTagsFromResource (Text)
rtfrmResourceName = lens _rtfrmResourceName (\s a -> s { _rtfrmResourceName = a })
{-# INLINE rtfrmResourceName #-}

-- | The tag key (name) of the tag to be removed.
rtfrmTagKeys :: Lens' RemoveTagsFromResource ([Text])
rtfrmTagKeys = lens _rtfrmTagKeys (\s a -> s { _rtfrmTagKeys = a })
{-# INLINE rtfrmTagKeys #-}

instance ToQuery RemoveTagsFromResource where
    toQuery = genericQuery def

data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RemoveTagsFromResource where
    type Sv RemoveTagsFromResource = RDS
    type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse

    request = post "RemoveTagsFromResource"
    response _ = nullaryResponse RemoveTagsFromResourceResponse
