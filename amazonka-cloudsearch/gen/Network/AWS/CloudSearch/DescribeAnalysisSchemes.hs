{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeAnalysisSchemes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets the analysis schemes configured for a domain. An analysis scheme
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
    , dassrqDeployed
    , dassrqAnalysisSchemeNames
    , dassrqDomainName

    -- * Response
    , DescribeAnalysisSchemesResponse
    -- ** Response constructor
    , describeAnalysisSchemesResponse
    -- ** Response lenses
    , dasrsStatus
    , dasrsAnalysisSchemes
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DescribeAnalysisSchemes@ operation.
-- Specifies the name of the domain you want to describe. To limit the
-- response to particular analysis schemes, specify the names of the
-- analysis schemes you want to describe. To show the active configuration
-- and exclude any pending changes, set the @Deployed@ option to @true@.
--
-- /See:/ 'describeAnalysisSchemes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dassrqDeployed'
--
-- * 'dassrqAnalysisSchemeNames'
--
-- * 'dassrqDomainName'
data DescribeAnalysisSchemes = DescribeAnalysisSchemes'
    { _dassrqDeployed            :: !(Maybe Bool)
    , _dassrqAnalysisSchemeNames :: !(Maybe [Text])
    , _dassrqDomainName          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAnalysisSchemes' smart constructor.
describeAnalysisSchemes :: Text -> DescribeAnalysisSchemes
describeAnalysisSchemes pDomainName =
    DescribeAnalysisSchemes'
    { _dassrqDeployed = Nothing
    , _dassrqAnalysisSchemeNames = Nothing
    , _dassrqDomainName = pDomainName
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
dassrqDeployed :: Lens' DescribeAnalysisSchemes (Maybe Bool)
dassrqDeployed = lens _dassrqDeployed (\ s a -> s{_dassrqDeployed = a});

-- | The analysis schemes you want to describe.
dassrqAnalysisSchemeNames :: Lens' DescribeAnalysisSchemes [Text]
dassrqAnalysisSchemeNames = lens _dassrqAnalysisSchemeNames (\ s a -> s{_dassrqAnalysisSchemeNames = a}) . _Default;

-- | The name of the domain you want to describe.
dassrqDomainName :: Lens' DescribeAnalysisSchemes Text
dassrqDomainName = lens _dassrqDomainName (\ s a -> s{_dassrqDomainName = a});

instance AWSRequest DescribeAnalysisSchemes where
        type Sv DescribeAnalysisSchemes = CloudSearch
        type Rs DescribeAnalysisSchemes =
             DescribeAnalysisSchemesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeAnalysisSchemesResult"
              (\ s h x ->
                 DescribeAnalysisSchemesResponse' <$>
                   (pure (fromEnum s)) <*>
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
               "Deployed" =: _dassrqDeployed,
               "AnalysisSchemeNames" =:
                 toQuery
                   (toQueryList "member" <$>
                      _dassrqAnalysisSchemeNames),
               "DomainName" =: _dassrqDomainName]

-- | The result of a @DescribeAnalysisSchemes@ request. Contains the analysis
-- schemes configured for the domain specified in the request.
--
-- /See:/ 'describeAnalysisSchemesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasrsStatus'
--
-- * 'dasrsAnalysisSchemes'
data DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse'
    { _dasrsStatus          :: !Int
    , _dasrsAnalysisSchemes :: ![AnalysisSchemeStatus]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAnalysisSchemesResponse' smart constructor.
describeAnalysisSchemesResponse :: Int -> DescribeAnalysisSchemesResponse
describeAnalysisSchemesResponse pStatus =
    DescribeAnalysisSchemesResponse'
    { _dasrsStatus = pStatus
    , _dasrsAnalysisSchemes = mempty
    }

-- | FIXME: Undocumented member.
dasrsStatus :: Lens' DescribeAnalysisSchemesResponse Int
dasrsStatus = lens _dasrsStatus (\ s a -> s{_dasrsStatus = a});

-- | The analysis scheme descriptions.
dasrsAnalysisSchemes :: Lens' DescribeAnalysisSchemesResponse [AnalysisSchemeStatus]
dasrsAnalysisSchemes = lens _dasrsAnalysisSchemes (\ s a -> s{_dasrsAnalysisSchemes = a});
