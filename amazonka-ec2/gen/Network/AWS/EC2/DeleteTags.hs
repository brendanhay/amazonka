{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified set of tags from the specified set of resources. This
-- call is designed to follow a DescribeTags request. For more information
-- about tags, see Tagging Your Resources in the Amazon Elastic Compute Cloud
-- User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteTags.html>
module Network.AWS.EC2.DeleteTags
    (
    -- * Request
      DeleteTags
    -- ** Request constructor
    , deleteTags
    -- ** Request lenses
    , dt1DryRun
    , dt1Resources
    , dt1Tags

    -- * Response
    , DeleteTagsResponse
    -- ** Response constructor
    , deleteTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteTags = DeleteTags
    { _dt1DryRun    :: Maybe Bool
    , _dt1Resources :: List "ResourceId" Text
    , _dt1Tags      :: List "item" Tag
    } deriving (Eq, Show)

-- | 'DeleteTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dt1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dt1Resources' @::@ ['Text']
--
-- * 'dt1Tags' @::@ ['Tag']
--
deleteTags :: DeleteTags
deleteTags = DeleteTags
    { _dt1DryRun    = Nothing
    , _dt1Resources = mempty
    , _dt1Tags      = mempty
    }

dt1DryRun :: Lens' DeleteTags (Maybe Bool)
dt1DryRun = lens _dt1DryRun (\s a -> s { _dt1DryRun = a })

-- | The ID of the resource. For example, ami-1a2b3c4d. You can specify more
-- than one resource ID.
dt1Resources :: Lens' DeleteTags [Text]
dt1Resources = lens _dt1Resources (\s a -> s { _dt1Resources = a }) . _List

-- | One or more tags to delete. If you omit the value parameter, we delete
-- the tag regardless of its value. If you specify this parameter with an
-- empty string as the value, we delete the key only if its value is an
-- empty string.
dt1Tags :: Lens' DeleteTags [Tag]
dt1Tags = lens _dt1Tags (\s a -> s { _dt1Tags = a }) . _List

data DeleteTagsResponse = DeleteTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteTagsResponse' constructor.
deleteTagsResponse :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse

instance ToPath DeleteTags where
    toPath = const "/"

instance ToQuery DeleteTags where
    toQuery DeleteTags{..} = mconcat
        [ "dryRun"     =? _dt1DryRun
        , "resourceId" =? _dt1Resources
        , "tag"        =? _dt1Tags
        ]

instance ToHeaders DeleteTags

instance AWSRequest DeleteTags where
    type Sv DeleteTags = EC2
    type Rs DeleteTags = DeleteTagsResponse

    request  = post "DeleteTags"
    response = nullResponse DeleteTagsResponse
