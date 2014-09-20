{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DefineSuggester
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
module Network.AWS.CloudSearch.DefineSuggester
    (
    -- * Request
      DefineSuggester
    -- ** Request constructor
    , defineSuggester
    -- ** Request lenses
    , ds1DomainName
    , ds1Suggester

    -- * Response
    , DefineSuggesterResponse
    -- ** Response constructor
    , defineSuggesterResponse
    -- ** Response lenses
    , dsrSuggester
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DefineSuggester operation. Specifies
-- the name of the domain you want to update and the suggester configuration.
data DefineSuggester = DefineSuggester
    { _ds1DomainName :: Text
    , _ds1Suggester :: Suggester
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DefineSuggester' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @Suggester ::@ @Suggester@
--
defineSuggester :: Text -- ^ 'ds1DomainName'
                -> Suggester -- ^ 'ds1Suggester'
                -> DefineSuggester
defineSuggester p1 p2 = DefineSuggester
    { _ds1DomainName = p1
    , _ds1Suggester = p2
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
ds1DomainName :: Lens' DefineSuggester Text
ds1DomainName = lens _ds1DomainName (\s a -> s { _ds1DomainName = a })

-- | Configuration information for a search suggester. Each suggester has a
-- unique name and specifies the text field you want to use for suggestions.
-- The following options can be configured for a suggester: FuzzyMatching,
-- SortExpression.
ds1Suggester :: Lens' DefineSuggester Suggester
ds1Suggester = lens _ds1Suggester (\s a -> s { _ds1Suggester = a })

instance ToQuery DefineSuggester where
    toQuery = genericQuery def

-- | The result of a DefineSuggester request. Contains the status of the
-- newly-configured suggester.
newtype DefineSuggesterResponse = DefineSuggesterResponse
    { _dsrSuggester :: SuggesterStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DefineSuggesterResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Suggester ::@ @SuggesterStatus@
--
defineSuggesterResponse :: SuggesterStatus -- ^ 'dsrSuggester'
                        -> DefineSuggesterResponse
defineSuggesterResponse p1 = DefineSuggesterResponse
    { _dsrSuggester = p1
    }

-- | The value of a Suggester and its current status.
dsrSuggester :: Lens' DefineSuggesterResponse SuggesterStatus
dsrSuggester = lens _dsrSuggester (\s a -> s { _dsrSuggester = a })

instance FromXML DefineSuggesterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DefineSuggester where
    type Sv DefineSuggester = CloudSearch
    type Rs DefineSuggester = DefineSuggesterResponse

    request = post "DefineSuggester"
    response _ = xmlResponse
