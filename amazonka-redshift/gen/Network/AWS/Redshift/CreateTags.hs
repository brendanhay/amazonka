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

-- Module      : Network.AWS.Redshift.CreateTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds one or more tags to a specified resource. A resource can have up to 10
-- tags. If you try to create more than 10 tags for a resource, you will
-- receive an error and the attempt will fail. If you specify a key that
-- already exists for the resource, the value for that key will be updated
-- with the new value.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateTags.html>
module Network.AWS.Redshift.CreateTags
    (
    -- * Request
      CreateTags
    -- ** Request constructor
    , createTags
    -- ** Request lenses
    , ctResourceName
    , ctTags

    -- * Response
    , CreateTagsResponse
    -- ** Response constructor
    , createTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data CreateTags = CreateTags
    { _ctResourceName :: Text
    , _ctTags         :: List "Tag" Tag
    } deriving (Eq, Show)

-- | 'CreateTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctResourceName' @::@ 'Text'
--
-- * 'ctTags' @::@ ['Tag']
--
createTags :: Text -- ^ 'ctResourceName'
           -> CreateTags
createTags p1 = CreateTags
    { _ctResourceName = p1
    , _ctTags         = mempty
    }

-- | The Amazon Resource Name (ARN) to which you want to add the tag or tags.
-- For example, @arn:aws:redshift:us-east-1:123456789:cluster:t1@.
ctResourceName :: Lens' CreateTags Text
ctResourceName = lens _ctResourceName (\s a -> s { _ctResourceName = a })

-- | One or more name/value pairs to add as tags to the specified resource.
-- Each tag name is passed in with the parameter @tag-key@ and the
-- corresponding value is passed in with the parameter @tag-value@. The
-- @tag-key@ and @tag-value@ parameters are separated by a colon (:).
-- Separate multiple tags with a space. For example, @--tags
-- "tag-key"="owner":"tag-value"="admin"
-- "tag-key"="environment":"tag-value"="test"
-- "tag-key"="version":"tag-value"="1.0"@.
ctTags :: Lens' CreateTags [Tag]
ctTags = lens _ctTags (\s a -> s { _ctTags = a }) . _List

data CreateTagsResponse = CreateTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateTagsResponse' constructor.
createTagsResponse :: CreateTagsResponse
createTagsResponse = CreateTagsResponse

instance ToPath CreateTags where
    toPath = const "/"

instance ToQuery CreateTags where
    toQuery CreateTags{..} = mconcat
        [ "ResourceName" =? _ctResourceName
        , "Tags"         =? _ctTags
        ]

instance ToHeaders CreateTags

instance AWSRequest CreateTags where
    type Sv CreateTags = Redshift
    type Rs CreateTags = CreateTagsResponse

    request  = post "CreateTags"
    response = nullResponse CreateTagsResponse
