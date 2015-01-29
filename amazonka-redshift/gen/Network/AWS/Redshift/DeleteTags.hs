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

-- Module      : Network.AWS.Redshift.DeleteTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a tag or tags from a resource. You must provide the ARN of the
-- resource from which you want to delete the tag or tags.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteTags.html>
module Network.AWS.Redshift.DeleteTags
    (
    -- * Request
      DeleteTags
    -- ** Request constructor
    , deleteTags
    -- ** Request lenses
    , dt1ResourceName
    , dt1TagKeys

    -- * Response
    , DeleteTagsResponse
    -- ** Response constructor
    , deleteTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DeleteTags = DeleteTags
    { _dt1ResourceName :: Text
    , _dt1TagKeys      :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dt1ResourceName' @::@ 'Text'
--
-- * 'dt1TagKeys' @::@ ['Text']
--
deleteTags :: Text -- ^ 'dt1ResourceName'
           -> DeleteTags
deleteTags p1 = DeleteTags
    { _dt1ResourceName = p1
    , _dt1TagKeys      = mempty
    }

-- | The Amazon Resource Name (ARN) from which you want to remove the tag or
-- tags. For example, 'arn:aws:redshift:us-east-1:123456789:cluster:t1'.
dt1ResourceName :: Lens' DeleteTags Text
dt1ResourceName = lens _dt1ResourceName (\s a -> s { _dt1ResourceName = a })

-- | The tag key that you want to delete.
dt1TagKeys :: Lens' DeleteTags [Text]
dt1TagKeys = lens _dt1TagKeys (\s a -> s { _dt1TagKeys = a }) . _List

data DeleteTagsResponse = DeleteTagsResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteTagsResponse' constructor.
deleteTagsResponse :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse

instance ToPath DeleteTags where
    toPath = const "/"

instance ToQuery DeleteTags where
    toQuery DeleteTags{..} = mconcat
        [ "ResourceName" =? _dt1ResourceName
        , "TagKeys"      =? _dt1TagKeys
        ]

instance ToHeaders DeleteTags

instance AWSRequest DeleteTags where
    type Sv DeleteTags = Redshift
    type Rs DeleteTags = DeleteTagsResponse

    request  = post "DeleteTags"
    response = nullResponse DeleteTagsResponse
