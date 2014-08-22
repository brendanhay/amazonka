{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.CloudSearch.V2013_01_01.DefineAnalysisScheme where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

data DefineAnalysisScheme = DefineAnalysisScheme
    { _dasvAnalysisScheme :: AnalysisScheme
      -- ^ Configuration information for an analysis scheme. Each analysis
      -- scheme has a unique name and specifies the language of the text
      -- to be processed. The following options can be configured for an
      -- analysis scheme: Synonyms, Stopwords, StemmingDictionary, and
      -- AlgorithmicStemming.
    , _dasvDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    } deriving (Show, Generic)

makeLenses ''DefineAnalysisScheme

instance ToQuery DefineAnalysisScheme where
    toQuery = genericQuery def

data DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse
    { _daswAnalysisScheme :: AnalysisSchemeStatus
      -- ^ The status and configuration of an AnalysisScheme.
    } deriving (Show, Generic)

makeLenses ''DefineAnalysisSchemeResponse

instance FromXML DefineAnalysisSchemeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DefineAnalysisScheme where
    type Sv DefineAnalysisScheme = CloudSearch
    type Rs DefineAnalysisScheme = DefineAnalysisSchemeResponse

    request = post "DefineAnalysisScheme"
    response _ = xmlResponse
