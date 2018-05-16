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
-- Module      : Network.AWS.EC2.DeleteTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of tags from the specified set of resources.
--
--
-- To list the current tags, use 'DescribeTags' . For more information about tags, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.DeleteTags
    (
    -- * Creating a Request
      deleteTags
    , DeleteTags
    -- * Request Lenses
    , dtsDryRun
    , dtsTags
    , dtsResources

    -- * Destructuring the Response
    , deleteTagsResponse
    , DeleteTagsResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DeleteTags.
--
--
--
-- /See:/ 'deleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { _dtsDryRun    :: !(Maybe Bool)
  , _dtsTags      :: !(Maybe [Tag])
  , _dtsResources :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtsTags' - One or more tags to delete. Specify a tag key and an optional tag value to delete specific tags. If you specify a tag key without a tag value, we delete any tag with this key regardless of its value. If you specify a tag key with an empty string as the tag value, we delete the tag only if its value is an empty string. If you omit this parameter, we delete all user-defined tags for the specified resources. We do not delete AWS-generated tags (tags that have the @aws:@ prefix).
--
-- * 'dtsResources' - The IDs of one or more resources.
deleteTags
    :: DeleteTags
deleteTags =
  DeleteTags' {_dtsDryRun = Nothing, _dtsTags = Nothing, _dtsResources = mempty}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtsDryRun :: Lens' DeleteTags (Maybe Bool)
dtsDryRun = lens _dtsDryRun (\ s a -> s{_dtsDryRun = a})

-- | One or more tags to delete. Specify a tag key and an optional tag value to delete specific tags. If you specify a tag key without a tag value, we delete any tag with this key regardless of its value. If you specify a tag key with an empty string as the tag value, we delete the tag only if its value is an empty string. If you omit this parameter, we delete all user-defined tags for the specified resources. We do not delete AWS-generated tags (tags that have the @aws:@ prefix).
dtsTags :: Lens' DeleteTags [Tag]
dtsTags = lens _dtsTags (\ s a -> s{_dtsTags = a}) . _Default . _Coerce

-- | The IDs of one or more resources.
dtsResources :: Lens' DeleteTags [Text]
dtsResources = lens _dtsResources (\ s a -> s{_dtsResources = a}) . _Coerce

instance AWSRequest DeleteTags where
        type Rs DeleteTags = DeleteTagsResponse
        request = postQuery ec2
        response = receiveNull DeleteTagsResponse'

instance Hashable DeleteTags where

instance NFData DeleteTags where

instance ToHeaders DeleteTags where
        toHeaders = const mempty

instance ToPath DeleteTags where
        toPath = const "/"

instance ToQuery DeleteTags where
        toQuery DeleteTags'{..}
          = mconcat
              ["Action" =: ("DeleteTags" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dtsDryRun,
               toQuery (toQueryList "Tag" <$> _dtsTags),
               toQueryList "ResourceId" _dtsResources]

-- | /See:/ 'deleteTagsResponse' smart constructor.
data DeleteTagsResponse =
  DeleteTagsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
--
deleteTagsResponse
    :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse'


instance NFData DeleteTagsResponse where
