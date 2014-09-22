{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.AddTagsToResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds metadata tags to an Amazon RDS resource. These tags can also be used
-- with cost allocation reporting to track cost associated with Amazon RDS
-- resources, or used in Condition statement in IAM policy for Amazon RDS. For
-- an overview on tagging Amazon RDS resources, see Tagging Amazon RDS
-- Resources.
module Network.AWS.RDS.AddTagsToResource
    (
    -- * Request
      AddTagsToResource
    -- ** Request constructor
    , addTagsToResource
    -- ** Request lenses
    , attrResourceName
    , attrTag

    -- * Response
    , AddTagsToResourceResponse
    -- ** Response constructor
    , addTagsToResourceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data AddTagsToResource = AddTagsToResource
    { _attrResourceName :: Text
    , _attrTag :: [Tag]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddTagsToResource' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceName ::@ @Text@
--
-- * @Tag ::@ @[Tag]@
--
addTagsToResource :: Text -- ^ 'attrResourceName'
                  -> [Tag] -- ^ 'attrTag'
                  -> AddTagsToResource
addTagsToResource p1 p2 = AddTagsToResource
    { _attrResourceName = p1
    , _attrTag = p2
    }

-- | The Amazon RDS resource the tags will be added to. This value is an Amazon
-- Resource Name (ARN). For information about creating an ARN, see
-- Constructing an RDS Amazon Resource Name (ARN).
attrResourceName :: Lens' AddTagsToResource Text
attrResourceName =
    lens _attrResourceName (\s a -> s { _attrResourceName = a })

-- | The tags to be assigned to the Amazon RDS resource.
attrTag :: Lens' AddTagsToResource [Tag]
attrTag = lens _attrTag (\s a -> s { _attrTag = a })

instance ToQuery AddTagsToResource where
    toQuery = genericQuery def

data AddTagsToResourceResponse = AddTagsToResourceResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddTagsToResourceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
addTagsToResourceResponse :: AddTagsToResourceResponse
addTagsToResourceResponse = AddTagsToResourceResponse

instance AWSRequest AddTagsToResource where
    type Sv AddTagsToResource = RDS
    type Rs AddTagsToResource = AddTagsToResourceResponse

    request = post "AddTagsToResource"
    response _ = nullaryResponse AddTagsToResourceResponse
