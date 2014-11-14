{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.Kinesis.RemoveTagsFromStream
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes tags from the specified Amazon Kinesis stream. If you specify a tag
-- that does not exist, it is ignored.
module Network.AWS.Kinesis.RemoveTagsFromStream
    (
    -- * Request
      RemoveTagsFromStream
    -- ** Request constructor
    , removeTagsFromStream
    -- ** Request lenses
    , rtfsStreamName
    , rtfsTagKeys

    -- * Response
    , RemoveTagsFromStreamResponse
    -- ** Response constructor
    , removeTagsFromStreamResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Kinesis.Types
import qualified GHC.Exts

data RemoveTagsFromStream = RemoveTagsFromStream
    { _rtfsStreamName :: Text
    , _rtfsTagKeys    :: List1 Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RemoveTagsFromStream' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtfsStreamName' @::@ 'Text'
--
-- * 'rtfsTagKeys' @::@ 'NonEmpty' 'Text'
--
removeTagsFromStream :: Text -- ^ 'rtfsStreamName'
                     -> NonEmpty Text -- ^ 'rtfsTagKeys'
                     -> RemoveTagsFromStream
removeTagsFromStream p1 p2 = RemoveTagsFromStream
    { _rtfsStreamName = p1
    , _rtfsTagKeys    = withIso _List1 (const id) p2
    }

-- | The name of the stream.
rtfsStreamName :: Lens' RemoveTagsFromStream Text
rtfsStreamName = lens _rtfsStreamName (\s a -> s { _rtfsStreamName = a })

-- | A list of tag keys. Each corresponding tag is removed from the stream.
rtfsTagKeys :: Lens' RemoveTagsFromStream (NonEmpty Text)
rtfsTagKeys = lens _rtfsTagKeys (\s a -> s { _rtfsTagKeys = a })
    . _List1

instance ToPath RemoveTagsFromStream where
    toPath = const "/"

instance ToQuery RemoveTagsFromStream where
    toQuery = const mempty

instance ToHeaders RemoveTagsFromStream

instance ToBody RemoveTagsFromStream where
    toBody = toBody . encode . _rtfsStreamName

data RemoveTagsFromStreamResponse = RemoveTagsFromStreamResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RemoveTagsFromStreamResponse' constructor.
removeTagsFromStreamResponse :: RemoveTagsFromStreamResponse
removeTagsFromStreamResponse = RemoveTagsFromStreamResponse

instance AWSRequest RemoveTagsFromStream where
    type Sv RemoveTagsFromStream = Kinesis
    type Rs RemoveTagsFromStream = RemoveTagsFromStreamResponse

    request  = post
    response = nullaryResponse RemoveTagsFromStreamResponse
