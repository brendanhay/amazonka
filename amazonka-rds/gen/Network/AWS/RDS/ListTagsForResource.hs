{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.RDS.ListTagsForResource
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
module Network.AWS.RDS.ListTagsForResource
    (
    -- * Request
      ListTagsForResourceMessage
    -- ** Request constructor
    , listTagsForResourceMessage
    -- ** Request lenses
    , ltfrmFilters
    , ltfrmResourceName

    -- * Response
    , TagListMessage
    -- ** Response constructor
    , tagListMessage
    -- ** Response lenses
    , tlmTagList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data ListTagsForResourceMessage = ListTagsForResourceMessage
    { _ltfrmFilters      :: [Filter]
    , _ltfrmResourceName :: Text
    } deriving (Eq, Show, Generic)

-- | 'ListTagsForResourceMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrmFilters' @::@ ['Filter']
--
-- * 'ltfrmResourceName' @::@ 'Text'
--
listTagsForResourceMessage :: Text -- ^ 'ltfrmResourceName'
                           -> ListTagsForResourceMessage
listTagsForResourceMessage p1 = ListTagsForResourceMessage
    { _ltfrmResourceName = p1
    , _ltfrmFilters      = mempty
    }

-- | This parameter is not currently supported.
ltfrmFilters :: Lens' ListTagsForResourceMessage [Filter]
ltfrmFilters = lens _ltfrmFilters (\s a -> s { _ltfrmFilters = a })

-- | The Amazon RDS resource with tags to be listed. This value is an Amazon
-- Resource Name (ARN). For information about creating an ARN, see
-- Constructing an RDS Amazon Resource Name (ARN).
ltfrmResourceName :: Lens' ListTagsForResourceMessage Text
ltfrmResourceName =
    lens _ltfrmResourceName (\s a -> s { _ltfrmResourceName = a })

instance ToQuery ListTagsForResourceMessage

instance ToPath ListTagsForResourceMessage where
    toPath = const "/"

newtype TagListMessage = TagListMessage
    { _tlmTagList :: [Tag]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList TagListMessage
    type Item TagListMessage = Tag

    fromList = TagListMessage . fromList
    toList   = toList . _tlmTagList

-- | 'TagListMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tlmTagList' @::@ ['Tag']
--
tagListMessage :: TagListMessage
tagListMessage = TagListMessage
    { _tlmTagList = mempty
    }

-- | List of tags returned by the ListTagsForResource operation.
tlmTagList :: Lens' TagListMessage [Tag]
tlmTagList = lens _tlmTagList (\s a -> s { _tlmTagList = a })

instance FromXML TagListMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TagListMessage"

instance AWSRequest ListTagsForResourceMessage where
    type Sv ListTagsForResourceMessage = RDS
    type Rs ListTagsForResourceMessage = TagListMessage

    request  = post "ListTagsForResource"
    response = xmlResponse $ \h x -> TagListMessage
        <$> x %| "TagList"
