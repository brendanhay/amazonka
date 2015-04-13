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

-- Module      : Network.AWS.Route53Domains.UpdateTagsForDomain
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

-- | This operation adds or updates tags for a specified domain.
--
-- All tag operations are eventually consistent; subsequent operations may not
-- immediately represent all issued operations.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-UpdateTagsForDomain.html>
module Network.AWS.Route53Domains.UpdateTagsForDomain
    (
    -- * Request
      UpdateTagsForDomain
    -- ** Request constructor
    , updateTagsForDomain
    -- ** Request lenses
    , utfdDomainName
    , utfdTagsToUpdate

    -- * Response
    , UpdateTagsForDomainResponse
    -- ** Response constructor
    , updateTagsForDomainResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

data UpdateTagsForDomain = UpdateTagsForDomain
    { _utfdDomainName   :: Text
    , _utfdTagsToUpdate :: List "TagsToUpdate" Tag
    } deriving (Eq, Read, Show)

-- | 'UpdateTagsForDomain' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utfdDomainName' @::@ 'Text'
--
-- * 'utfdTagsToUpdate' @::@ ['Tag']
--
updateTagsForDomain :: Text -- ^ 'utfdDomainName'
                    -> UpdateTagsForDomain
updateTagsForDomain p1 = UpdateTagsForDomain
    { _utfdDomainName   = p1
    , _utfdTagsToUpdate = mempty
    }

-- | The domain for which you want to add or update tags.
--
-- The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z, the
-- numbers 0 through 9, and hyphen (-). Hyphens are allowed only when theyaposre
-- surrounded by letters, numbers, or other hyphens. You canapost specify a
-- hyphen at the beginning or end of a label. To specify an Internationalized
-- Domain Name, you must convert the name to Punycode.
--
-- Required: Yes
utfdDomainName :: Lens' UpdateTagsForDomain Text
utfdDomainName = lens _utfdDomainName (\s a -> s { _utfdDomainName = a })

-- | A list of the tag keys and values that you want to add or update. If you
-- specify a key that already exists, the corresponding value will be replaced.
--
-- Type: A complex type containing a list of tags
--
-- Default: None
--
-- Required: No
--
-- '> Each tag includes the following elements:
--
-- Key
--
-- The key (name) of a tag.
--
-- Type: String
--
-- Default: None
--
-- Valid values: Unicode characters including alphanumeric, space, and
-- ".:/=+\-%@"
--
-- Constraints: Each key can be 1-128 characters long.
--
-- Required: Yes
--
-- Value
--
-- The value of a tag.
--
-- Type: String
--
-- Default: None
--
-- Valid values: Unicode characters including alphanumeric, space, and
-- ".:/=+\-%@"
--
-- Constraints: Each value can be 0-256 characters long.
--
-- Required: Yes
--
--
utfdTagsToUpdate :: Lens' UpdateTagsForDomain [Tag]
utfdTagsToUpdate = lens _utfdTagsToUpdate (\s a -> s { _utfdTagsToUpdate = a }) . _List

data UpdateTagsForDomainResponse = UpdateTagsForDomainResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'UpdateTagsForDomainResponse' constructor.
updateTagsForDomainResponse :: UpdateTagsForDomainResponse
updateTagsForDomainResponse = UpdateTagsForDomainResponse

instance ToPath UpdateTagsForDomain where
    toPath = const "/"

instance ToQuery UpdateTagsForDomain where
    toQuery = const mempty

instance ToHeaders UpdateTagsForDomain

instance ToJSON UpdateTagsForDomain where
    toJSON UpdateTagsForDomain{..} = object
        [ "DomainName"   .= _utfdDomainName
        , "TagsToUpdate" .= _utfdTagsToUpdate
        ]

instance AWSRequest UpdateTagsForDomain where
    type Sv UpdateTagsForDomain = Route53Domains
    type Rs UpdateTagsForDomain = UpdateTagsForDomainResponse

    request  = post "UpdateTagsForDomain"
    response = nullResponse UpdateTagsForDomainResponse
