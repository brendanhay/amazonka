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
    , removeTagsFromResource
    -- ** Request lenses
    , rtfrmTagKeys
    , rtfrmResourceName

    -- * Response
    , RemoveTagsFromResourceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RemoveTagsFromResource' request.
removeTagsFromResource :: [Text] -- ^ 'rtfrmTagKeys'
                       -> Text -- ^ 'rtfrmResourceName'
                       -> RemoveTagsFromResource
removeTagsFromResource p1 p2 = RemoveTagsFromResource
    { _rtfrmTagKeys = p1
    , _rtfrmResourceName = p2
    }
{-# INLINE removeTagsFromResource #-}

data RemoveTagsFromResource = RemoveTagsFromResource
    { _rtfrmTagKeys :: [Text]
      -- ^ The tag key (name) of the tag to be removed.
    , _rtfrmResourceName :: Text
      -- ^ The Amazon RDS resource the tags will be removed from. This value
      -- is an Amazon Resource Name (ARN). For information about creating
      -- an ARN, see Constructing an RDS Amazon Resource Name (ARN).
    } deriving (Show, Generic)

-- | The tag key (name) of the tag to be removed.
rtfrmTagKeys :: Lens' RemoveTagsFromResource ([Text])
rtfrmTagKeys f x =
    f (_rtfrmTagKeys x)
        <&> \y -> x { _rtfrmTagKeys = y }
{-# INLINE rtfrmTagKeys #-}

-- | The Amazon RDS resource the tags will be removed from. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- Constructing an RDS Amazon Resource Name (ARN).
rtfrmResourceName :: Lens' RemoveTagsFromResource (Text)
rtfrmResourceName f x =
    f (_rtfrmResourceName x)
        <&> \y -> x { _rtfrmResourceName = y }
{-# INLINE rtfrmResourceName #-}

instance ToQuery RemoveTagsFromResource where
    toQuery = genericQuery def

data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RemoveTagsFromResource where
    type Sv RemoveTagsFromResource = RDS
    type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse

    request = post "RemoveTagsFromResource"
    response _ = nullaryResponse RemoveTagsFromResourceResponse
