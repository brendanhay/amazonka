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
-- Module      : Network.AWS.RDS.RemoveTagsFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes metadata tags from an Amazon RDS resource.
--
--
-- For an overview on tagging an Amazon RDS resource, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources> in the /Amazon RDS User Guide./
module Network.AWS.RDS.RemoveTagsFromResource
  ( -- * Creating a Request
    removeTagsFromResource,
    RemoveTagsFromResource,

    -- * Request Lenses
    rtfrResourceName,
    rtfrTagKeys,

    -- * Destructuring the Response
    removeTagsFromResourceResponse,
    RemoveTagsFromResourceResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'removeTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { _rtfrResourceName ::
      !Text,
    _rtfrTagKeys :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveTagsFromResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfrResourceName' - The Amazon RDS resource that the tags are removed from. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide./
--
-- * 'rtfrTagKeys' - The tag key (name) of the tag to be removed.
removeTagsFromResource ::
  -- | 'rtfrResourceName'
  Text ->
  RemoveTagsFromResource
removeTagsFromResource pResourceName_ =
  RemoveTagsFromResource'
    { _rtfrResourceName = pResourceName_,
      _rtfrTagKeys = mempty
    }

-- | The Amazon RDS resource that the tags are removed from. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide./
rtfrResourceName :: Lens' RemoveTagsFromResource Text
rtfrResourceName = lens _rtfrResourceName (\s a -> s {_rtfrResourceName = a})

-- | The tag key (name) of the tag to be removed.
rtfrTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeys = lens _rtfrTagKeys (\s a -> s {_rtfrTagKeys = a}) . _Coerce

instance AWSRequest RemoveTagsFromResource where
  type Rs RemoveTagsFromResource = RemoveTagsFromResourceResponse
  request = postQuery rds
  response = receiveNull RemoveTagsFromResourceResponse'

instance Hashable RemoveTagsFromResource

instance NFData RemoveTagsFromResource

instance ToHeaders RemoveTagsFromResource where
  toHeaders = const mempty

instance ToPath RemoveTagsFromResource where
  toPath = const "/"

instance ToQuery RemoveTagsFromResource where
  toQuery RemoveTagsFromResource' {..} =
    mconcat
      [ "Action" =: ("RemoveTagsFromResource" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "ResourceName" =: _rtfrResourceName,
        "TagKeys" =: toQueryList "member" _rtfrTagKeys
      ]

-- | /See:/ 'removeTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveTagsFromResourceResponse' with the minimum fields required to make a request.
removeTagsFromResourceResponse ::
  RemoveTagsFromResourceResponse
removeTagsFromResourceResponse = RemoveTagsFromResourceResponse'

instance NFData RemoveTagsFromResourceResponse
