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

-- Module      : Network.AWS.Route53Domains.ListTagsForDomain
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

-- | This operation returns all of the tags that are associated with the specified
-- domain.
--
-- All tag operations are eventually consistent; subsequent operations may not
-- immediately represent all issued operations.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-ListTagsForDomain.html>
module Network.AWS.Route53Domains.ListTagsForDomain
    (
    -- * Request
      ListTagsForDomain
    -- ** Request constructor
    , listTagsForDomain
    -- ** Request lenses
    , ltfdDomainName

    -- * Response
    , ListTagsForDomainResponse
    -- ** Response constructor
    , listTagsForDomainResponse
    -- ** Response lenses
    , ltfdrTagList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

newtype ListTagsForDomain = ListTagsForDomain
    { _ltfdDomainName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'ListTagsForDomain' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfdDomainName' @::@ 'Text'
--
listTagsForDomain :: Text -- ^ 'ltfdDomainName'
                  -> ListTagsForDomain
listTagsForDomain p1 = ListTagsForDomain
    { _ltfdDomainName = p1
    }

-- | The domain for which you want to get a list of tags.
ltfdDomainName :: Lens' ListTagsForDomain Text
ltfdDomainName = lens _ltfdDomainName (\s a -> s { _ltfdDomainName = a })

newtype ListTagsForDomainResponse = ListTagsForDomainResponse
    { _ltfdrTagList :: List "TagList" Tag
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList ListTagsForDomainResponse where
    type Item ListTagsForDomainResponse = Tag

    fromList = ListTagsForDomainResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ltfdrTagList

-- | 'ListTagsForDomainResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfdrTagList' @::@ ['Tag']
--
listTagsForDomainResponse :: ListTagsForDomainResponse
listTagsForDomainResponse = ListTagsForDomainResponse
    { _ltfdrTagList = mempty
    }

-- | A list of the tags that are associated with the specified domain.
--
-- Type: A complex type containing a list of tags
--
-- Each tag includes the following elements.
--
-- Key
--
-- The key (name) of a tag.
--
-- Type: String
--
-- Value
--
-- The value of a tag.
--
-- Type: String
--
--
ltfdrTagList :: Lens' ListTagsForDomainResponse [Tag]
ltfdrTagList = lens _ltfdrTagList (\s a -> s { _ltfdrTagList = a }) . _List

instance ToPath ListTagsForDomain where
    toPath = const "/"

instance ToQuery ListTagsForDomain where
    toQuery = const mempty

instance ToHeaders ListTagsForDomain

instance ToJSON ListTagsForDomain where
    toJSON ListTagsForDomain{..} = object
        [ "DomainName" .= _ltfdDomainName
        ]

instance AWSRequest ListTagsForDomain where
    type Sv ListTagsForDomain = Route53Domains
    type Rs ListTagsForDomain = ListTagsForDomainResponse

    request  = post "ListTagsForDomain"
    response = jsonResponse

instance FromJSON ListTagsForDomainResponse where
    parseJSON = withObject "ListTagsForDomainResponse" $ \o -> ListTagsForDomainResponse
        <$> o .:? "TagList" .!= mempty
