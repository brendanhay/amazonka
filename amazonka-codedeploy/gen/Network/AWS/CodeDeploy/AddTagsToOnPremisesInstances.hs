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

-- Module      : Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
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

-- | Adds tags to on-premises instances.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_AddTagsToOnPremisesInstances.html>
module Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
    (
    -- * Request
      AddTagsToOnPremisesInstances
    -- ** Request constructor
    , addTagsToOnPremisesInstances
    -- ** Request lenses
    , attopiInstanceNames
    , attopiTags

    -- * Response
    , AddTagsToOnPremisesInstancesResponse
    -- ** Response constructor
    , addTagsToOnPremisesInstancesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data AddTagsToOnPremisesInstances = AddTagsToOnPremisesInstances
    { _attopiInstanceNames :: List "instanceNames" Text
    , _attopiTags          :: List "tags" Tag
    } deriving (Eq, Read, Show)

-- | 'AddTagsToOnPremisesInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attopiInstanceNames' @::@ ['Text']
--
-- * 'attopiTags' @::@ ['Tag']
--
addTagsToOnPremisesInstances :: AddTagsToOnPremisesInstances
addTagsToOnPremisesInstances = AddTagsToOnPremisesInstances
    { _attopiTags          = mempty
    , _attopiInstanceNames = mempty
    }

-- | The names of the on-premises instances to add tags to.
attopiInstanceNames :: Lens' AddTagsToOnPremisesInstances [Text]
attopiInstanceNames =
    lens _attopiInstanceNames (\s a -> s { _attopiInstanceNames = a })
        . _List

-- | The tag key-value pairs to add to the on-premises instances.
--
-- Keys and values are both required. Keys cannot be nulls or empty strings.
-- Value-only tags are not allowed.
attopiTags :: Lens' AddTagsToOnPremisesInstances [Tag]
attopiTags = lens _attopiTags (\s a -> s { _attopiTags = a }) . _List

data AddTagsToOnPremisesInstancesResponse = AddTagsToOnPremisesInstancesResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AddTagsToOnPremisesInstancesResponse' constructor.
addTagsToOnPremisesInstancesResponse :: AddTagsToOnPremisesInstancesResponse
addTagsToOnPremisesInstancesResponse = AddTagsToOnPremisesInstancesResponse

instance ToPath AddTagsToOnPremisesInstances where
    toPath = const "/"

instance ToQuery AddTagsToOnPremisesInstances where
    toQuery = const mempty

instance ToHeaders AddTagsToOnPremisesInstances

instance ToJSON AddTagsToOnPremisesInstances where
    toJSON AddTagsToOnPremisesInstances{..} = object
        [ "tags"          .= _attopiTags
        , "instanceNames" .= _attopiInstanceNames
        ]

instance AWSRequest AddTagsToOnPremisesInstances where
    type Sv AddTagsToOnPremisesInstances = CodeDeploy
    type Rs AddTagsToOnPremisesInstances = AddTagsToOnPremisesInstancesResponse

    request  = post "AddTagsToOnPremisesInstances"
    response = nullResponse AddTagsToOnPremisesInstancesResponse
