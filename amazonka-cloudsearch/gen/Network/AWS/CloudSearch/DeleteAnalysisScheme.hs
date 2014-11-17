{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DeleteAnalysisScheme
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
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DeleteAnalysisScheme.html>
module Network.AWS.CloudSearch.DeleteAnalysisScheme
    (
    -- * Request
      DeleteAnalysisScheme
    -- ** Request constructor
    , deleteAnalysisScheme
    -- ** Request lenses
    , dasAnalysisSchemeName
    , dasDomainName

    -- * Response
    , DeleteAnalysisSchemeResponse
    -- ** Response constructor
    , deleteAnalysisSchemeResponse
    -- ** Response lenses
    , dasrAnalysisScheme
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data DeleteAnalysisScheme = DeleteAnalysisScheme
    { _dasAnalysisSchemeName :: Text
    , _dasDomainName         :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteAnalysisScheme' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasAnalysisSchemeName' @::@ 'Text'
--
-- * 'dasDomainName' @::@ 'Text'
--
deleteAnalysisScheme :: Text -- ^ 'dasDomainName'
                     -> Text -- ^ 'dasAnalysisSchemeName'
                     -> DeleteAnalysisScheme
deleteAnalysisScheme p1 p2 = DeleteAnalysisScheme
    { _dasDomainName         = p1
    , _dasAnalysisSchemeName = p2
    }

-- | The name of the analysis scheme you want to delete.
dasAnalysisSchemeName :: Lens' DeleteAnalysisScheme Text
dasAnalysisSchemeName =
    lens _dasAnalysisSchemeName (\s a -> s { _dasAnalysisSchemeName = a })

dasDomainName :: Lens' DeleteAnalysisScheme Text
dasDomainName = lens _dasDomainName (\s a -> s { _dasDomainName = a })

newtype DeleteAnalysisSchemeResponse = DeleteAnalysisSchemeResponse
    { _dasrAnalysisScheme :: AnalysisSchemeStatus
    } deriving (Eq, Show, Generic)

-- | 'DeleteAnalysisSchemeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasrAnalysisScheme' @::@ 'AnalysisSchemeStatus'
--
deleteAnalysisSchemeResponse :: AnalysisSchemeStatus -- ^ 'dasrAnalysisScheme'
                             -> DeleteAnalysisSchemeResponse
deleteAnalysisSchemeResponse p1 = DeleteAnalysisSchemeResponse
    { _dasrAnalysisScheme = p1
    }

-- | The status of the analysis scheme being deleted.
dasrAnalysisScheme :: Lens' DeleteAnalysisSchemeResponse AnalysisSchemeStatus
dasrAnalysisScheme =
    lens _dasrAnalysisScheme (\s a -> s { _dasrAnalysisScheme = a })

instance AWSRequest DeleteAnalysisScheme where
    type Sv DeleteAnalysisScheme = CloudSearch
    type Rs DeleteAnalysisScheme = DeleteAnalysisSchemeResponse

    request  = post "DeleteAnalysisScheme"
    response = xmlResponse

instance FromXML DeleteAnalysisSchemeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteAnalysisSchemeResponse"

instance ToPath DeleteAnalysisScheme where
    toPath = const "/"

instance ToHeaders DeleteAnalysisScheme

instance ToQuery DeleteAnalysisScheme
