{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DeleteTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between configuration items and one or more tags. This API accepts a list of multiple configuration items.
--
--
module Network.AWS.Discovery.DeleteTags
    (
    -- * Creating a Request
      deleteTags
    , DeleteTags
    -- * Request Lenses
    , dtTags
    , dtConfigurationIds

    -- * Destructuring the Response
    , deleteTagsResponse
    , DeleteTagsResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { _dtTags             :: !(Maybe [Tag])
  , _dtConfigurationIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTags' - Tags that you want to delete from one or more configuration items. Specify the tags that you want to delete in a /key/ -/value/ format. For example: @{"key": "serverType", "value": "webServer"}@
--
-- * 'dtConfigurationIds' - A list of configuration items with tags that you want to delete.
deleteTags
    :: DeleteTags
deleteTags = DeleteTags' {_dtTags = Nothing, _dtConfigurationIds = mempty}


-- | Tags that you want to delete from one or more configuration items. Specify the tags that you want to delete in a /key/ -/value/ format. For example: @{"key": "serverType", "value": "webServer"}@
dtTags :: Lens' DeleteTags [Tag]
dtTags = lens _dtTags (\ s a -> s{_dtTags = a}) . _Default . _Coerce

-- | A list of configuration items with tags that you want to delete.
dtConfigurationIds :: Lens' DeleteTags [Text]
dtConfigurationIds = lens _dtConfigurationIds (\ s a -> s{_dtConfigurationIds = a}) . _Coerce

instance AWSRequest DeleteTags where
        type Rs DeleteTags = DeleteTagsResponse
        request = postJSON discovery
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteTagsResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteTags where

instance NFData DeleteTags where

instance ToHeaders DeleteTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.DeleteTags" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTags where
        toJSON DeleteTags'{..}
          = object
              (catMaybes
                 [("tags" .=) <$> _dtTags,
                  Just ("configurationIds" .= _dtConfigurationIds)])

instance ToPath DeleteTags where
        toPath = const "/"

instance ToQuery DeleteTags where
        toQuery = const mempty

-- | /See:/ 'deleteTagsResponse' smart constructor.
newtype DeleteTagsResponse = DeleteTagsResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteTagsResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteTagsResponse
deleteTagsResponse pResponseStatus_ =
  DeleteTagsResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteTagsResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteTagsResponse where
