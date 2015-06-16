{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudSearch.DescribeAnalysisSchemes
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets the analysis schemes configured for a domain. An analysis scheme
-- defines language-specific text processing options for a @text@ field.
-- Can be limited to specific analysis schemes by name. By default, shows
-- all analysis schemes and includes any pending changes to the
-- configuration. Set the @Deployed@ option to @true@ to show the active
-- configuration and exclude pending changes. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeAnalysisSchemes.html>
module Network.AWS.CloudSearch.DescribeAnalysisSchemes
    (
    -- * Request
      DescribeAnalysisSchemes
    -- ** Request constructor
    , describeAnalysisSchemes
    -- ** Request lenses
    , descDeployed
    , descAnalysisSchemeNames
    , descDomainName

    -- * Response
    , DescribeAnalysisSchemesResponse
    -- ** Response constructor
    , describeAnalysisSchemesResponse
    -- ** Response lenses
    , dasrAnalysisSchemes
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudSearch.Types

-- | /See:/ 'describeAnalysisSchemes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'descDeployed'
--
-- * 'descAnalysisSchemeNames'
--
-- * 'descDomainName'
data DescribeAnalysisSchemes = DescribeAnalysisSchemes'{_descDeployed :: Maybe Bool, _descAnalysisSchemeNames :: Maybe [Text], _descDomainName :: Text} deriving (Eq, Read, Show)

-- | 'DescribeAnalysisSchemes' smart constructor.
describeAnalysisSchemes :: Text -> DescribeAnalysisSchemes
describeAnalysisSchemes pDomainName = DescribeAnalysisSchemes'{_descDeployed = Nothing, _descAnalysisSchemeNames = Nothing, _descDomainName = pDomainName};

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
descDeployed :: Lens' DescribeAnalysisSchemes (Maybe Bool)
descDeployed = lens _descDeployed (\ s a -> s{_descDeployed = a});

-- | The analysis schemes you want to describe.
descAnalysisSchemeNames :: Lens' DescribeAnalysisSchemes [Text]
descAnalysisSchemeNames = lens _descAnalysisSchemeNames (\ s a -> s{_descAnalysisSchemeNames = a}) . _Default;

-- | The name of the domain you want to describe.
descDomainName :: Lens' DescribeAnalysisSchemes Text
descDomainName = lens _descDomainName (\ s a -> s{_descDomainName = a});

instance AWSRequest DescribeAnalysisSchemes where
        type Sv DescribeAnalysisSchemes = CloudSearch
        type Rs DescribeAnalysisSchemes =
             DescribeAnalysisSchemesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeAnalysisSchemesResult"
              (\ s h x ->
                 DescribeAnalysisSchemesResponse' <$>
                   (x .@? "AnalysisSchemes" .!@ mempty >>=
                      parseXMLList "member"))

instance ToHeaders DescribeAnalysisSchemes where
        toHeaders = const mempty

instance ToPath DescribeAnalysisSchemes where
        toPath = const "/"

instance ToQuery DescribeAnalysisSchemes where
        toQuery DescribeAnalysisSchemes'{..}
          = mconcat
              ["Action" =:
                 ("DescribeAnalysisSchemes" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "Deployed" =: _descDeployed,
               "AnalysisSchemeNames" =:
                 toQuery
                   (toQueryList "member" <$> _descAnalysisSchemeNames),
               "DomainName" =: _descDomainName]

-- | /See:/ 'describeAnalysisSchemesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasrAnalysisSchemes'
newtype DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse'{_dasrAnalysisSchemes :: [AnalysisSchemeStatus]} deriving (Eq, Read, Show)

-- | 'DescribeAnalysisSchemesResponse' smart constructor.
describeAnalysisSchemesResponse :: DescribeAnalysisSchemesResponse
describeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse'{_dasrAnalysisSchemes = mempty};

-- | The analysis scheme descriptions.
dasrAnalysisSchemes :: Lens' DescribeAnalysisSchemesResponse [AnalysisSchemeStatus]
dasrAnalysisSchemes = lens _dasrAnalysisSchemes (\ s a -> s{_dasrAnalysisSchemes = a});
