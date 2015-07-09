{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteAnalysisScheme
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes an analysis scheme. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DeleteAnalysisScheme.html>
module Network.AWS.CloudSearch.DeleteAnalysisScheme
    (
    -- * Request
      DeleteAnalysisScheme
    -- ** Request constructor
    , deleteAnalysisScheme
    -- ** Request lenses
    , dasDomainName
    , dasAnalysisSchemeName

    -- * Response
    , DeleteAnalysisSchemeResponse
    -- ** Response constructor
    , deleteAnalysisSchemeResponse
    -- ** Response lenses
    , dStatus
    , dAnalysisScheme
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DeleteAnalysisScheme@ operation.
-- Specifies the name of the domain you want to update and the analysis
-- scheme you want to delete.
--
-- /See:/ 'deleteAnalysisScheme' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasDomainName'
--
-- * 'dasAnalysisSchemeName'
data DeleteAnalysisScheme = DeleteAnalysisScheme'
    { _dasDomainName         :: !Text
    , _dasAnalysisSchemeName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAnalysisScheme' smart constructor.
deleteAnalysisScheme :: Text -> Text -> DeleteAnalysisScheme
deleteAnalysisScheme pDomainName pAnalysisSchemeName =
    DeleteAnalysisScheme'
    { _dasDomainName = pDomainName
    , _dasAnalysisSchemeName = pAnalysisSchemeName
    }

-- | FIXME: Undocumented member.
dasDomainName :: Lens' DeleteAnalysisScheme Text
dasDomainName = lens _dasDomainName (\ s a -> s{_dasDomainName = a});

-- | The name of the analysis scheme you want to delete.
dasAnalysisSchemeName :: Lens' DeleteAnalysisScheme Text
dasAnalysisSchemeName = lens _dasAnalysisSchemeName (\ s a -> s{_dasAnalysisSchemeName = a});

instance AWSRequest DeleteAnalysisScheme where
        type Sv DeleteAnalysisScheme = CloudSearch
        type Rs DeleteAnalysisScheme =
             DeleteAnalysisSchemeResponse
        request = post
        response
          = receiveXMLWrapper "DeleteAnalysisSchemeResult"
              (\ s h x ->
                 DeleteAnalysisSchemeResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "AnalysisScheme"))

instance ToHeaders DeleteAnalysisScheme where
        toHeaders = const mempty

instance ToPath DeleteAnalysisScheme where
        toPath = const "/"

instance ToQuery DeleteAnalysisScheme where
        toQuery DeleteAnalysisScheme'{..}
          = mconcat
              ["Action" =: ("DeleteAnalysisScheme" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _dasDomainName,
               "AnalysisSchemeName" =: _dasAnalysisSchemeName]

-- | The result of a @DeleteAnalysisScheme@ request. Contains the status of
-- the deleted analysis scheme.
--
-- /See:/ 'deleteAnalysisSchemeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dStatus'
--
-- * 'dAnalysisScheme'
data DeleteAnalysisSchemeResponse = DeleteAnalysisSchemeResponse'
    { _dStatus         :: !Int
    , _dAnalysisScheme :: !AnalysisSchemeStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAnalysisSchemeResponse' smart constructor.
deleteAnalysisSchemeResponse :: Int -> AnalysisSchemeStatus -> DeleteAnalysisSchemeResponse
deleteAnalysisSchemeResponse pStatus pAnalysisScheme =
    DeleteAnalysisSchemeResponse'
    { _dStatus = pStatus
    , _dAnalysisScheme = pAnalysisScheme
    }

-- | FIXME: Undocumented member.
dStatus :: Lens' DeleteAnalysisSchemeResponse Int
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});

-- | The status of the analysis scheme being deleted.
dAnalysisScheme :: Lens' DeleteAnalysisSchemeResponse AnalysisSchemeStatus
dAnalysisScheme = lens _dAnalysisScheme (\ s a -> s{_dAnalysisScheme = a});
