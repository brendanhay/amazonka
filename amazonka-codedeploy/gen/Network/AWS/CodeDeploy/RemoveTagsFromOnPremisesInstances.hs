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

-- Module      : Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
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

-- | Removes one or more tags from one or more on-premises instances.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_RemoveTagsFromOnPremisesInstances.html>
module Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
    (
    -- * Request
      RemoveTagsFromOnPremisesInstances
    -- ** Request constructor
    , removeTagsFromOnPremisesInstances
    -- ** Request lenses
    , rtfopiInstanceNames
    , rtfopiTags

    -- * Response
    , RemoveTagsFromOnPremisesInstancesResponse
    -- ** Response constructor
    , removeTagsFromOnPremisesInstancesResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data RemoveTagsFromOnPremisesInstances = RemoveTagsFromOnPremisesInstances
    { _rtfopiInstanceNames :: List "instanceNames" Text
    , _rtfopiTags          :: List "tags" Tag
    } deriving (Eq, Read, Show)

-- | 'RemoveTagsFromOnPremisesInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtfopiInstanceNames' @::@ ['Text']
--
-- * 'rtfopiTags' @::@ ['Tag']
--
removeTagsFromOnPremisesInstances :: RemoveTagsFromOnPremisesInstances
removeTagsFromOnPremisesInstances = RemoveTagsFromOnPremisesInstances
    { _rtfopiTags          = mempty
    , _rtfopiInstanceNames = mempty
    }

-- | The names of the on-premises instances to remove tags from.
rtfopiInstanceNames :: Lens' RemoveTagsFromOnPremisesInstances [Text]
rtfopiInstanceNames =
    lens _rtfopiInstanceNames (\s a -> s { _rtfopiInstanceNames = a })
        . _List

-- | The tag key-value pairs to remove from the on-premises instances.
rtfopiTags :: Lens' RemoveTagsFromOnPremisesInstances [Tag]
rtfopiTags = lens _rtfopiTags (\s a -> s { _rtfopiTags = a }) . _List

data RemoveTagsFromOnPremisesInstancesResponse = RemoveTagsFromOnPremisesInstancesResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'RemoveTagsFromOnPremisesInstancesResponse' constructor.
removeTagsFromOnPremisesInstancesResponse :: RemoveTagsFromOnPremisesInstancesResponse
removeTagsFromOnPremisesInstancesResponse = RemoveTagsFromOnPremisesInstancesResponse

instance ToPath RemoveTagsFromOnPremisesInstances where
    toPath = const "/"

instance ToQuery RemoveTagsFromOnPremisesInstances where
    toQuery = const mempty

instance ToHeaders RemoveTagsFromOnPremisesInstances

instance ToJSON RemoveTagsFromOnPremisesInstances where
    toJSON RemoveTagsFromOnPremisesInstances{..} = object
        [ "tags"          .= _rtfopiTags
        , "instanceNames" .= _rtfopiInstanceNames
        ]

instance AWSRequest RemoveTagsFromOnPremisesInstances where
    type Sv RemoveTagsFromOnPremisesInstances = CodeDeploy
    type Rs RemoveTagsFromOnPremisesInstances = RemoveTagsFromOnPremisesInstancesResponse

    request  = post "RemoveTagsFromOnPremisesInstances"
    response = nullResponse RemoveTagsFromOnPremisesInstancesResponse
