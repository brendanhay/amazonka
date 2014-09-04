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
    , mkDefineAnalysisSchemeRequest
    -- ** Request lenses
    , dasrDomainName
    , dasrAnalysisScheme

    -- * Response
    , DefineAnalysisSchemeResponse
    -- ** Response lenses
    , dassAnalysisScheme
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DefineAnalysisScheme' request.
mkDefineAnalysisSchemeRequest :: Text -- ^ 'dasrDomainName'
                              -> AnalysisScheme -- ^ 'dasrAnalysisScheme'
                              -> DefineAnalysisScheme
mkDefineAnalysisSchemeRequest p1 p2 = DefineAnalysisScheme
    { _dasrDomainName = p1
    , _dasrAnalysisScheme = p2
    }
{-# INLINE mkDefineAnalysisSchemeRequest #-}

data DefineAnalysisScheme = DefineAnalysisScheme
    { _dasrDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _dasrAnalysisScheme :: AnalysisScheme
      -- ^ Configuration information for an analysis scheme. Each analysis
      -- scheme has a unique name and specifies the language of the text
      -- to be processed. The following options can be configured for an
      -- analysis scheme: Synonyms, Stopwords, StemmingDictionary, and
      -- AlgorithmicStemming.
    } deriving (Show, Generic)

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
dasrDomainName :: Lens' DefineAnalysisScheme (Text)
dasrDomainName = lens _dasrDomainName (\s a -> s { _dasrDomainName = a })
{-# INLINE dasrDomainName #-}

-- | Configuration information for an analysis scheme. Each analysis scheme has
-- a unique name and specifies the language of the text to be processed. The
-- following options can be configured for an analysis scheme: Synonyms,
-- Stopwords, StemmingDictionary, and AlgorithmicStemming.
dasrAnalysisScheme :: Lens' DefineAnalysisScheme (AnalysisScheme)
dasrAnalysisScheme = lens _dasrAnalysisScheme (\s a -> s { _dasrAnalysisScheme = a })
{-# INLINE dasrAnalysisScheme #-}

instance ToQuery DefineAnalysisScheme where
    toQuery = genericQuery def

newtype DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse
    { _dassAnalysisScheme :: AnalysisSchemeStatus
      -- ^ The status and configuration of an AnalysisScheme.
    } deriving (Show, Generic)

-- | The status and configuration of an AnalysisScheme.
dassAnalysisScheme :: Lens' DefineAnalysisSchemeResponse (AnalysisSchemeStatus)
dassAnalysisScheme = lens _dassAnalysisScheme (\s a -> s { _dassAnalysisScheme = a })
{-# INLINE dassAnalysisScheme #-}

instance FromXML DefineAnalysisSchemeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DefineAnalysisScheme where
    type Sv DefineAnalysisScheme = CloudSearch
    type Rs DefineAnalysisScheme = DefineAnalysisSchemeResponse

    request = post "DefineAnalysisScheme"
    response _ = xmlResponse
