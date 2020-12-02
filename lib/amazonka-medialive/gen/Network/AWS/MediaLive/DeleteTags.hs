{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags for a resource
module Network.AWS.MediaLive.DeleteTags
  ( -- * Creating a Request
    deleteTags,
    DeleteTags,

    -- * Request Lenses
    dtTagKeys,
    dtResourceARN,

    -- * Destructuring the Response
    deleteTagsResponse,
    DeleteTagsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DeleteTagsRequest
--
-- /See:/ 'deleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { _dtTagKeys :: ![Text],
    _dtResourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTagKeys' - An array of tag keys to delete
--
-- * 'dtResourceARN' - Undocumented member.
deleteTags ::
  -- | 'dtResourceARN'
  Text ->
  DeleteTags
deleteTags pResourceARN_ =
  DeleteTags' {_dtTagKeys = mempty, _dtResourceARN = pResourceARN_}

-- | An array of tag keys to delete
dtTagKeys :: Lens' DeleteTags [Text]
dtTagKeys = lens _dtTagKeys (\s a -> s {_dtTagKeys = a}) . _Coerce

-- | Undocumented member.
dtResourceARN :: Lens' DeleteTags Text
dtResourceARN = lens _dtResourceARN (\s a -> s {_dtResourceARN = a})

instance AWSRequest DeleteTags where
  type Rs DeleteTags = DeleteTagsResponse
  request = delete mediaLive
  response = receiveNull DeleteTagsResponse'

instance Hashable DeleteTags

instance NFData DeleteTags

instance ToHeaders DeleteTags where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteTags where
  toPath DeleteTags' {..} =
    mconcat ["/prod/tags/", toBS _dtResourceARN]

instance ToQuery DeleteTags where
  toQuery DeleteTags' {..} =
    mconcat ["tagKeys" =: toQueryList "member" _dtTagKeys]

-- | /See:/ 'deleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
deleteTagsResponse ::
  DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse'

instance NFData DeleteTagsResponse
