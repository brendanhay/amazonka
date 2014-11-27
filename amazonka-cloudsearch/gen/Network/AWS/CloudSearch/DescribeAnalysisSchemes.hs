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

-- Module      : Network.AWS.CloudSearch.DescribeAnalysisSchemes
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

-- | Gets the analysis schemes configured for a domain. An analysis scheme defines
-- language-specific text processing options for a 'text' field. Can be limited to
-- specific analysis schemes by name. By default, shows all analysis schemes and
-- includes any pending changes to the configuration. Set the 'Deployed' option to 'true' to show the active configuration and exclude pending changes. For more
-- information, see Configuring Analysis Schemes in the /Amazon CloudSearchDeveloper Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeAnalysisSchemes.html>
module Network.AWS.CloudSearch.DescribeAnalysisSchemes
    (
    -- * Request
      DescribeAnalysisSchemes
    -- ** Request constructor
    , describeAnalysisSchemes
    -- ** Request lenses
    , das1AnalysisSchemeNames
    , das1Deployed
    , das1DomainName

    -- * Response
    , DescribeAnalysisSchemesResponse
    -- ** Response constructor
    , describeAnalysisSchemesResponse
    -- ** Response lenses
    , dasrAnalysisSchemes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data DescribeAnalysisSchemes = DescribeAnalysisSchemes
    { _das1AnalysisSchemeNames :: List "member" Text
    , _das1Deployed            :: Maybe Bool
    , _das1DomainName          :: Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeAnalysisSchemes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'das1AnalysisSchemeNames' @::@ ['Text']
--
-- * 'das1Deployed' @::@ 'Maybe' 'Bool'
--
-- * 'das1DomainName' @::@ 'Text'
--
describeAnalysisSchemes :: Text -- ^ 'das1DomainName'
                        -> DescribeAnalysisSchemes
describeAnalysisSchemes p1 = DescribeAnalysisSchemes
    { _das1DomainName          = p1
    , _das1AnalysisSchemeNames = mempty
    , _das1Deployed            = Nothing
    }

-- | The analysis schemes you want to describe.
das1AnalysisSchemeNames :: Lens' DescribeAnalysisSchemes [Text]
das1AnalysisSchemeNames =
    lens _das1AnalysisSchemeNames (\s a -> s { _das1AnalysisSchemeNames = a })
        . _List

-- | Whether to display the deployed configuration ('true') or include any pending
-- changes ('false'). Defaults to 'false'.
das1Deployed :: Lens' DescribeAnalysisSchemes (Maybe Bool)
das1Deployed = lens _das1Deployed (\s a -> s { _das1Deployed = a })

-- | The name of the domain you want to describe.
das1DomainName :: Lens' DescribeAnalysisSchemes Text
das1DomainName = lens _das1DomainName (\s a -> s { _das1DomainName = a })

newtype DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse
    { _dasrAnalysisSchemes :: List "member" AnalysisSchemeStatus
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeAnalysisSchemesResponse where
    type Item DescribeAnalysisSchemesResponse = AnalysisSchemeStatus

    fromList = DescribeAnalysisSchemesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dasrAnalysisSchemes

-- | 'DescribeAnalysisSchemesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasrAnalysisSchemes' @::@ ['AnalysisSchemeStatus']
--
describeAnalysisSchemesResponse :: DescribeAnalysisSchemesResponse
describeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse
    { _dasrAnalysisSchemes = mempty
    }

-- | The analysis scheme descriptions.
dasrAnalysisSchemes :: Lens' DescribeAnalysisSchemesResponse [AnalysisSchemeStatus]
dasrAnalysisSchemes =
    lens _dasrAnalysisSchemes (\s a -> s { _dasrAnalysisSchemes = a })
        . _List

instance ToPath DescribeAnalysisSchemes where
    toPath = const "/"

instance ToQuery DescribeAnalysisSchemes where
    toQuery DescribeAnalysisSchemes{..} = mconcat
        [ "AnalysisSchemeNames" =? _das1AnalysisSchemeNames
        , "Deployed"            =? _das1Deployed
        , "DomainName"          =? _das1DomainName
        ]

instance ToHeaders DescribeAnalysisSchemes

instance AWSRequest DescribeAnalysisSchemes where
    type Sv DescribeAnalysisSchemes = CloudSearch
    type Rs DescribeAnalysisSchemes = DescribeAnalysisSchemesResponse

    request  = post "DescribeAnalysisSchemes"
    response = xmlResponse

instance FromXML DescribeAnalysisSchemesResponse where
    parseXML = withElement "DescribeAnalysisSchemesResult" $ \x -> DescribeAnalysisSchemesResponse
        <$> x .@  "AnalysisSchemes"
