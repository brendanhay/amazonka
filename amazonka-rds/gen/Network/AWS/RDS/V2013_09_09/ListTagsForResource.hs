{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.ListTagsForResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all tags on an Amazon RDS resource. For an overview on tagging an
-- Amazon RDS resource, see Tagging Amazon RDS Resources.
module Network.AWS.RDS.V2013_09_09.ListTagsForResource
    (
    -- * Request
      ListTagsForResource
    -- ** Request constructor
    , mkListTagsForResourceMessage
    -- ** Request lenses
    , ltfrmResourceName

    -- * Response
    , ListTagsForResourceResponse
    -- ** Response lenses
    , tlmTagList
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTagsForResource' request.
mkListTagsForResourceMessage :: Text -- ^ 'ltfrmResourceName'
                             -> ListTagsForResource
mkListTagsForResourceMessage p1 = ListTagsForResource
    { _ltfrmResourceName = p1
    }
{-# INLINE mkListTagsForResourceMessage #-}

newtype ListTagsForResource = ListTagsForResource
    { _ltfrmResourceName :: Text
      -- ^ The Amazon RDS resource with tags to be listed. This value is an
      -- Amazon Resource Name (ARN). For information about creating an
      -- ARN, see Constructing an RDS Amazon Resource Name (ARN).
    } deriving (Show, Generic)

-- | The Amazon RDS resource with tags to be listed. This value is an Amazon
-- Resource Name (ARN). For information about creating an ARN, see
-- Constructing an RDS Amazon Resource Name (ARN).
ltfrmResourceName :: Lens' ListTagsForResource (Text)
ltfrmResourceName = lens _ltfrmResourceName (\s a -> s { _ltfrmResourceName = a })
{-# INLINE ltfrmResourceName #-}

instance ToQuery ListTagsForResource where
    toQuery = genericQuery def

newtype ListTagsForResourceResponse = ListTagsForResourceResponse
    { _tlmTagList :: [Tag]
      -- ^ List of tags returned by the ListTagsForResource operation.
    } deriving (Show, Generic)

-- | List of tags returned by the ListTagsForResource operation.
tlmTagList :: Lens' ListTagsForResourceResponse ([Tag])
tlmTagList = lens _tlmTagList (\s a -> s { _tlmTagList = a })
{-# INLINE tlmTagList #-}

instance FromXML ListTagsForResourceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListTagsForResource where
    type Sv ListTagsForResource = RDS
    type Rs ListTagsForResource = ListTagsForResourceResponse

    request = post "ListTagsForResource"
    response _ = xmlResponse
