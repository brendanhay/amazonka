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
module Network.AWS.RDS.RemoveTagsFromResource
    (
    -- * Request
      RemoveTagsFromResourceMessage
    -- ** Request constructor
    , removeTagsFromResource
    -- ** Request lenses
    , rtfrmResourceName
    , rtfrmTagKeys

    -- * Response
    , RemoveTagsFromResourceResponse
    -- ** Response constructor
    , removeTagsFromResourceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data RemoveTagsFromResourceMessage = RemoveTagsFromResourceMessage
    { _rtfrmResourceName :: Text
    , _rtfrmTagKeys      :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'RemoveTagsFromResourceMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtfrmResourceName' @::@ 'Text'
--
-- * 'rtfrmTagKeys' @::@ ['Text']
--
removeTagsFromResource :: Text -- ^ 'rtfrmResourceName'
                       -> RemoveTagsFromResourceMessage
removeTagsFromResource p1 = RemoveTagsFromResourceMessage
    { _rtfrmResourceName = p1
    , _rtfrmTagKeys      = mempty
    }

-- | The Amazon RDS resource the tags will be removed from. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- Constructing an RDS Amazon Resource Name (ARN).
rtfrmResourceName :: Lens' RemoveTagsFromResourceMessage Text
rtfrmResourceName =
    lens _rtfrmResourceName (\s a -> s { _rtfrmResourceName = a })

-- | The tag key (name) of the tag to be removed.
rtfrmTagKeys :: Lens' RemoveTagsFromResourceMessage [Text]
rtfrmTagKeys = lens _rtfrmTagKeys (\s a -> s { _rtfrmTagKeys = a })

instance ToPath RemoveTagsFromResourceMessage where
    toPath = const "/"

instance ToQuery RemoveTagsFromResourceMessage

data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse

-- | 'RemoveTagsFromResourceResponse' constructor.
removeTagsFromResourceResponse :: RemoveTagsFromResourceResponse
removeTagsFromResourceResponse = RemoveTagsFromResourceResponse

instance AWSRequest RemoveTagsFromResourceMessage where
    type Sv RemoveTagsFromResourceMessage = RDS
    type Rs RemoveTagsFromResourceMessage = RemoveTagsFromResourceResponse

    request  = post "RemoveTagsFromResource"
    response = const (nullaryResponse RemoveTagsFromResourceResponse)
