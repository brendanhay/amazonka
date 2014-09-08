{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DefineAnalysisScheme
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures an analysis scheme that can be applied to a text or text-array
-- field to define language-specific text processing options. For more
-- information, see Configuring Analysis Schemes in the Amazon CloudSearch
-- Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DefineAnalysisScheme
    (
    -- * Request
      DefineAnalysisScheme
    -- ** Request constructor
    , mkDefineAnalysisScheme
    -- ** Request lenses
    , dasDomainName
    , dasAnalysisScheme

    -- * Response
    , DefineAnalysisSchemeResponse
    -- ** Response constructor
    , mkDefineAnalysisSchemeResponse
    -- ** Response lenses
    , dasrAnalysisScheme
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DefineAnalysisScheme operation.
-- Specifies the name of the domain you want to update and the analysis scheme
-- configuration.
data DefineAnalysisScheme = DefineAnalysisScheme
    { _dasDomainName :: Text
    , _dasAnalysisScheme :: AnalysisScheme
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DefineAnalysisScheme' request.
mkDefineAnalysisScheme :: Text -- ^ 'dasDomainName'
                       -> AnalysisScheme -- ^ 'dasAnalysisScheme'
                       -> DefineAnalysisScheme
mkDefineAnalysisScheme p1 p2 = DefineAnalysisScheme
    { _dasDomainName = p1
    , _dasAnalysisScheme = p2
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
dasDomainName :: Lens' DefineAnalysisScheme Text
dasDomainName = lens _dasDomainName (\s a -> s { _dasDomainName = a })

-- | Configuration information for an analysis scheme. Each analysis scheme has
-- a unique name and specifies the language of the text to be processed. The
-- following options can be configured for an analysis scheme: Synonyms,
-- Stopwords, StemmingDictionary, and AlgorithmicStemming.
dasAnalysisScheme :: Lens' DefineAnalysisScheme AnalysisScheme
dasAnalysisScheme =
    lens _dasAnalysisScheme (\s a -> s { _dasAnalysisScheme = a })

instance ToQuery DefineAnalysisScheme where
    toQuery = genericQuery def

-- | The result of a DefineAnalysisScheme request. Contains the status of the
-- newly-configured analysis scheme.
newtype DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse
    { _dasrAnalysisScheme :: AnalysisSchemeStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DefineAnalysisSchemeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDefineAnalysisSchemeResponse :: AnalysisSchemeStatus -- ^ 'dasrAnalysisScheme'
                               -> DefineAnalysisSchemeResponse
mkDefineAnalysisSchemeResponse p1 = DefineAnalysisSchemeResponse
    { _dasrAnalysisScheme = p1
    }

-- | The status and configuration of an AnalysisScheme.
dasrAnalysisScheme :: Lens' DefineAnalysisSchemeResponse AnalysisSchemeStatus
dasrAnalysisScheme =
    lens _dasrAnalysisScheme (\s a -> s { _dasrAnalysisScheme = a })

instance FromXML DefineAnalysisSchemeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DefineAnalysisScheme where
    type Sv DefineAnalysisScheme = CloudSearch
    type Rs DefineAnalysisScheme = DefineAnalysisSchemeResponse

    request = post "DefineAnalysisScheme"
    response _ = xmlResponse
