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
    , descDeployed
    , descAnalysisSchemeNames
    , descDomainName

    -- * Response
    , DescribeAnalysisSchemesResponse
    -- ** Response constructor
    , describeAnalysisSchemesResponse
    -- ** Response lenses
    , dasrStatus
    , dasrAnalysisSchemes
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
-- * 'descDeployed'
--
-- * 'descAnalysisSchemeNames'
--
-- * 'descDomainName'
data DescribeAnalysisSchemes = DescribeAnalysisSchemes'
    { _descDeployed            :: !(Maybe Bool)
    , _descAnalysisSchemeNames :: !(Maybe [Text])
    , _descDomainName          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAnalysisSchemes' smart constructor.
describeAnalysisSchemes :: Text -> DescribeAnalysisSchemes
describeAnalysisSchemes pDomainName =
    DescribeAnalysisSchemes'
    { _descDeployed = Nothing
    , _descAnalysisSchemeNames = Nothing
    , _descDomainName = pDomainName
    }

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
               "Deployed" =: _descDeployed,
               "AnalysisSchemeNames" =:
                 toQuery
                   (toQueryList "member" <$> _descAnalysisSchemeNames),
               "DomainName" =: _descDomainName]

-- | The result of a @DescribeAnalysisSchemes@ request. Contains the analysis
-- schemes configured for the domain specified in the request.
--
-- /See:/ 'describeAnalysisSchemesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasrStatus'
--
-- * 'dasrAnalysisSchemes'
data DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse'
    { _dasrStatus          :: !Int
    , _dasrAnalysisSchemes :: ![AnalysisSchemeStatus]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAnalysisSchemesResponse' smart constructor.
describeAnalysisSchemesResponse :: Int -> DescribeAnalysisSchemesResponse
describeAnalysisSchemesResponse pStatus =
    DescribeAnalysisSchemesResponse'
    { _dasrStatus = pStatus
    , _dasrAnalysisSchemes = mempty
    }

-- | FIXME: Undocumented member.
dasrStatus :: Lens' DescribeAnalysisSchemesResponse Int
dasrStatus = lens _dasrStatus (\ s a -> s{_dasrStatus = a});

-- | The analysis scheme descriptions.
dasrAnalysisSchemes :: Lens' DescribeAnalysisSchemesResponse [AnalysisSchemeStatus]
dasrAnalysisSchemes = lens _dasrAnalysisSchemes (\ s a -> s{_dasrAnalysisSchemes = a});
