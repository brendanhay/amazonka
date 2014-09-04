{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DeleteAnalysisScheme
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an analysis scheme. For more information, see Configuring Analysis
-- Schemes in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DeleteAnalysisScheme
    (
    -- * Request
      DeleteAnalysisScheme
    -- ** Request constructor
    , mkDeleteAnalysisSchemeRequest
    -- ** Request lenses
    , dastDomainName
    , dastAnalysisSchemeName

    -- * Response
    , DeleteAnalysisSchemeResponse
    -- ** Response lenses
    , dasuAnalysisScheme
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAnalysisScheme' request.
mkDeleteAnalysisSchemeRequest :: Text -- ^ 'dastDomainName'
                              -> Text -- ^ 'dastAnalysisSchemeName'
                              -> DeleteAnalysisScheme
mkDeleteAnalysisSchemeRequest p1 p2 = DeleteAnalysisScheme
    { _dastDomainName = p1
    , _dastAnalysisSchemeName = p2
    }
{-# INLINE mkDeleteAnalysisSchemeRequest #-}

data DeleteAnalysisScheme = DeleteAnalysisScheme
    { _dastDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _dastAnalysisSchemeName :: Text
      -- ^ The name of the analysis scheme you want to delete.
    } deriving (Show, Generic)

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
dastDomainName :: Lens' DeleteAnalysisScheme (Text)
dastDomainName = lens _dastDomainName (\s a -> s { _dastDomainName = a })
{-# INLINE dastDomainName #-}

-- | The name of the analysis scheme you want to delete.
dastAnalysisSchemeName :: Lens' DeleteAnalysisScheme (Text)
dastAnalysisSchemeName = lens _dastAnalysisSchemeName (\s a -> s { _dastAnalysisSchemeName = a })
{-# INLINE dastAnalysisSchemeName #-}

instance ToQuery DeleteAnalysisScheme where
    toQuery = genericQuery def

newtype DeleteAnalysisSchemeResponse = DeleteAnalysisSchemeResponse
    { _dasuAnalysisScheme :: AnalysisSchemeStatus
      -- ^ The status of the analysis scheme being deleted.
    } deriving (Show, Generic)

-- | The status of the analysis scheme being deleted.
dasuAnalysisScheme :: Lens' DeleteAnalysisSchemeResponse (AnalysisSchemeStatus)
dasuAnalysisScheme = lens _dasuAnalysisScheme (\s a -> s { _dasuAnalysisScheme = a })
{-# INLINE dasuAnalysisScheme #-}

instance FromXML DeleteAnalysisSchemeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteAnalysisScheme where
    type Sv DeleteAnalysisScheme = CloudSearch
    type Rs DeleteAnalysisScheme = DeleteAnalysisSchemeResponse

    request = post "DeleteAnalysisScheme"
    response _ = xmlResponse
