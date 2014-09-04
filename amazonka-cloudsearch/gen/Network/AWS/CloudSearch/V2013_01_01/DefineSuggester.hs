{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DefineSuggester
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures a suggester for a domain. A suggester enables you to display
-- possible matches before users finish typing their queries. When you
-- configure a suggester, you must specify the name of the text field you want
-- to search for possible matches and a unique name for the suggester. For
-- more information, see Getting Search Suggestions in the Amazon CloudSearch
-- Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DefineSuggester
    (
    -- * Request
      DefineSuggester
    -- ** Request constructor
    , mkDefineSuggesterRequest
    -- ** Request lenses
    , dsrDomainName
    , dsrSuggester

    -- * Response
    , DefineSuggesterResponse
    -- ** Response lenses
    , dssSuggester
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DefineSuggester' request.
mkDefineSuggesterRequest :: Text -- ^ 'dsrDomainName'
                         -> Suggester -- ^ 'dsrSuggester'
                         -> DefineSuggester
mkDefineSuggesterRequest p1 p2 = DefineSuggester
    { _dsrDomainName = p1
    , _dsrSuggester = p2
    }
{-# INLINE mkDefineSuggesterRequest #-}

data DefineSuggester = DefineSuggester
    { _dsrDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _dsrSuggester :: Suggester
      -- ^ Configuration information for a search suggester. Each suggester
      -- has a unique name and specifies the text field you want to use
      -- for suggestions. The following options can be configured for a
      -- suggester: FuzzyMatching, SortExpression.
    } deriving (Show, Generic)

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
dsrDomainName :: Lens' DefineSuggester (Text)
dsrDomainName = lens _dsrDomainName (\s a -> s { _dsrDomainName = a })
{-# INLINE dsrDomainName #-}

-- | Configuration information for a search suggester. Each suggester has a
-- unique name and specifies the text field you want to use for suggestions.
-- The following options can be configured for a suggester: FuzzyMatching,
-- SortExpression.
dsrSuggester :: Lens' DefineSuggester (Suggester)
dsrSuggester = lens _dsrSuggester (\s a -> s { _dsrSuggester = a })
{-# INLINE dsrSuggester #-}

instance ToQuery DefineSuggester where
    toQuery = genericQuery def

newtype DefineSuggesterResponse = DefineSuggesterResponse
    { _dssSuggester :: SuggesterStatus
      -- ^ The value of a Suggester and its current status.
    } deriving (Show, Generic)

-- | The value of a Suggester and its current status.
dssSuggester :: Lens' DefineSuggesterResponse (SuggesterStatus)
dssSuggester = lens _dssSuggester (\s a -> s { _dssSuggester = a })
{-# INLINE dssSuggester #-}

instance FromXML DefineSuggesterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DefineSuggester where
    type Sv DefineSuggester = CloudSearch
    type Rs DefineSuggester = DefineSuggesterResponse

    request = post "DefineSuggester"
    response _ = xmlResponse
