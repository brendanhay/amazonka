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
    , mkDeleteAnalysisScheme
    -- ** Request lenses
    , das1DomainName
    , das1AnalysisSchemeName

    -- * Response
    , DeleteAnalysisSchemeResponse
    -- ** Response constructor
    , mkDeleteAnalysisSchemeResponse
    -- ** Response lenses
    , dasrrAnalysisScheme
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DeleteAnalysisScheme operation.
-- Specifies the name of the domain you want to update and the analysis scheme
-- you want to delete.
data DeleteAnalysisScheme = DeleteAnalysisScheme
    { _das1DomainName :: Text
    , _das1AnalysisSchemeName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAnalysisScheme' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @AnalysisSchemeName ::@ @Text@
--
mkDeleteAnalysisScheme :: Text -- ^ 'das1DomainName'
                       -> Text -- ^ 'das1AnalysisSchemeName'
                       -> DeleteAnalysisScheme
mkDeleteAnalysisScheme p1 p2 = DeleteAnalysisScheme
    { _das1DomainName = p1
    , _das1AnalysisSchemeName = p2
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
das1DomainName :: Lens' DeleteAnalysisScheme Text
das1DomainName = lens _das1DomainName (\s a -> s { _das1DomainName = a })

-- | The name of the analysis scheme you want to delete.
das1AnalysisSchemeName :: Lens' DeleteAnalysisScheme Text
das1AnalysisSchemeName =
    lens _das1AnalysisSchemeName (\s a -> s { _das1AnalysisSchemeName = a })

instance ToQuery DeleteAnalysisScheme where
    toQuery = genericQuery def

-- | The result of a DeleteAnalysisScheme request. Contains the status of the
-- deleted analysis scheme.
newtype DeleteAnalysisSchemeResponse = DeleteAnalysisSchemeResponse
    { _dasrrAnalysisScheme :: AnalysisSchemeStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAnalysisSchemeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AnalysisScheme ::@ @AnalysisSchemeStatus@
--
mkDeleteAnalysisSchemeResponse :: AnalysisSchemeStatus -- ^ 'dasrrAnalysisScheme'
                               -> DeleteAnalysisSchemeResponse
mkDeleteAnalysisSchemeResponse p1 = DeleteAnalysisSchemeResponse
    { _dasrrAnalysisScheme = p1
    }

-- | The status of the analysis scheme being deleted.
dasrrAnalysisScheme :: Lens' DeleteAnalysisSchemeResponse AnalysisSchemeStatus
dasrrAnalysisScheme =
    lens _dasrrAnalysisScheme (\s a -> s { _dasrrAnalysisScheme = a })

instance FromXML DeleteAnalysisSchemeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteAnalysisScheme where
    type Sv DeleteAnalysisScheme = CloudSearch
    type Rs DeleteAnalysisScheme = DeleteAnalysisSchemeResponse

    request = post "DeleteAnalysisScheme"
    response _ = xmlResponse
