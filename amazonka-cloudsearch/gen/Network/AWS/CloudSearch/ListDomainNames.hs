{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.ListDomainNames
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all search domains owned by an account.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_ListDomainNames.html>
module Network.AWS.CloudSearch.ListDomainNames
    (
    -- * Request
      ListDomainNames
    -- ** Request constructor
    , listDomainNames

    -- * Response
    , ListDomainNamesResponse
    -- ** Response constructor
    , listDomainNamesResponse
    -- ** Response lenses
    , ldnrDomainNames
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data ListDomainNames = ListDomainNames
    deriving (Eq, Ord, Show, Generic)

-- | 'ListDomainNames' constructor.
listDomainNames :: ListDomainNames
listDomainNames = ListDomainNames

newtype ListDomainNamesResponse = ListDomainNamesResponse
    { _ldnrDomainNames :: Map Text Text
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

-- | 'ListDomainNamesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldnrDomainNames' @::@ 'HashMap' 'Text' 'Text'
--
listDomainNamesResponse :: ListDomainNamesResponse
listDomainNamesResponse = ListDomainNamesResponse
    { _ldnrDomainNames = mempty
    }

-- | The names of the search domains owned by an account.
ldnrDomainNames :: Lens' ListDomainNamesResponse (HashMap Text Text)
ldnrDomainNames = lens _ldnrDomainNames (\s a -> s { _ldnrDomainNames = a })
    . _Map

instance ToPath ListDomainNames where
    toPath = const "/"

instance ToQuery ListDomainNames where
    toQuery = const mempty

instance ToHeaders ListDomainNames

instance AWSRequest ListDomainNames where
    type Sv ListDomainNames = CloudSearch
    type Rs ListDomainNames = ListDomainNamesResponse

    request  = post "ListDomainNames"
    response = xmlResponse

instance FromXML ListDomainNamesResponse where
    parseXML = withElement "ListDomainNamesResult" $ \x ->
            <$> x .@ "DomainNames"
