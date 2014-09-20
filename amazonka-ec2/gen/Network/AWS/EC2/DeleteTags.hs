{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
-- User Guide. Example This example deletes the tags for the AMI with the ID
-- ami-1a2b3c4d. First, get a list of the tags by using the DescribeTags
-- request, then delete them. https://ec2.amazonaws.com/?Action=DeleteTags
-- &amp;ResourceId.1=ami-1a2b3c4d &amp;Tag.1.Key=webserver
-- &amp;Tag.2.Key=stack &amp;AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE
-- true Example This example deletes the stack and webserver tags for two
-- particular instances. https://ec2.amazonaws.com/?Action=DeleteTags
-- &amp;ResourceId.1=i-5f4e3d2a &amp;ResourceId.2=i-5f4e3d2a
-- &amp;Tag.1.Key=stack &amp;Tag.2.Key=webserver &amp;AUTHPARAMS Example You
-- can specify a tag key without a corresponding tag value to delete the tag
-- regardless of its value. This example request deletes all tags that have a
-- key of Purpose, regardless of the tag value.
-- https://ec2.amazonaws.com/?Action=DeleteTags &amp;ResourceId.1=i-5f4e3d2a
-- &amp;Tag.1.Key=Purpose &amp;AUTHPARAMS Example When you create a tag, you
-- can set the tag value to the empty string. Correspondingly, you can delete
-- only tags that have a specific key and whose value is the empty string.
-- This example request deletes all tags for the specified instance where the
-- key is Purpose and the tag value is the empty string.
-- https://ec2.amazonaws.com/?Action=DeleteTags &amp;ResourceId.1=i-5f4e3d2a
-- &amp;Tag.1.Key=Purpose &amp;Tag.2.Value= &amp;AUTHPARAMS.
module Network.AWS.EC2.DeleteTags
    (
    -- * Request
      DeleteTags
    -- ** Request constructor
    , deleteTags
    -- ** Request lenses
    , dtResources
    , dtTags

    -- * Response
    , DeleteTagsResponse
    -- ** Response constructor
    , deleteTagsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DeleteTags = DeleteTags
    { _dtResources :: [Text]
    , _dtTags :: [Tag]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteTags' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Resources ::@ @[Text]@
--
-- * @Tags ::@ @[Tag]@
--
deleteTags :: [Text] -- ^ 'dtResources'
           -> DeleteTags
deleteTags p1 = DeleteTags
    { _dtResources = p1
    , _dtTags = mempty
    }

-- | The ID of the resource. For example, ami-1a2b3c4d. You can specify more
-- than one resource ID.
dtResources :: Lens' DeleteTags [Text]
dtResources = lens _dtResources (\s a -> s { _dtResources = a })

-- | One or more tags to delete. If you omit the value parameter, we delete the
-- tag regardless of its value. If you specify this parameter with an empty
-- string as the value, we delete the key only if its value is an empty
-- string.
dtTags :: Lens' DeleteTags [Tag]
dtTags = lens _dtTags (\s a -> s { _dtTags = a })

instance ToQuery DeleteTags where
    toQuery = genericQuery def

data DeleteTagsResponse = DeleteTagsResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteTagsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteTagsResponse :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse

instance AWSRequest DeleteTags where
    type Sv DeleteTags = EC2
    type Rs DeleteTags = DeleteTagsResponse

    request = post "DeleteTags"
    response _ = nullaryResponse DeleteTagsResponse
