{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , dasrqDomainName
    , dasrqAnalysisSchemeName

    -- * Response
    , DeleteAnalysisSchemeResponse
    -- ** Response constructor
    , deleteAnalysisSchemeResponse
    -- ** Response lenses
    , dassrsStatus
    , dassrsAnalysisScheme
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
-- * 'dasrqDomainName'
--
-- * 'dasrqAnalysisSchemeName'
data DeleteAnalysisScheme = DeleteAnalysisScheme'
    { _dasrqDomainName         :: !Text
    , _dasrqAnalysisSchemeName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAnalysisScheme' smart constructor.
deleteAnalysisScheme :: Text -> Text -> DeleteAnalysisScheme
deleteAnalysisScheme pDomainName_ pAnalysisSchemeName_ =
    DeleteAnalysisScheme'
    { _dasrqDomainName = pDomainName_
    , _dasrqAnalysisSchemeName = pAnalysisSchemeName_
    }

-- | FIXME: Undocumented member.
dasrqDomainName :: Lens' DeleteAnalysisScheme Text
dasrqDomainName = lens _dasrqDomainName (\ s a -> s{_dasrqDomainName = a});

-- | The name of the analysis scheme you want to delete.
dasrqAnalysisSchemeName :: Lens' DeleteAnalysisScheme Text
dasrqAnalysisSchemeName = lens _dasrqAnalysisSchemeName (\ s a -> s{_dasrqAnalysisSchemeName = a});

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
               "DomainName" =: _dasrqDomainName,
               "AnalysisSchemeName" =: _dasrqAnalysisSchemeName]

-- | The result of a @DeleteAnalysisScheme@ request. Contains the status of
-- the deleted analysis scheme.
--
-- /See:/ 'deleteAnalysisSchemeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dassrsStatus'
--
-- * 'dassrsAnalysisScheme'
data DeleteAnalysisSchemeResponse = DeleteAnalysisSchemeResponse'
    { _dassrsStatus         :: !Int
    , _dassrsAnalysisScheme :: !AnalysisSchemeStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAnalysisSchemeResponse' smart constructor.
deleteAnalysisSchemeResponse :: Int -> AnalysisSchemeStatus -> DeleteAnalysisSchemeResponse
deleteAnalysisSchemeResponse pStatus_ pAnalysisScheme_ =
    DeleteAnalysisSchemeResponse'
    { _dassrsStatus = pStatus_
    , _dassrsAnalysisScheme = pAnalysisScheme_
    }

-- | FIXME: Undocumented member.
dassrsStatus :: Lens' DeleteAnalysisSchemeResponse Int
dassrsStatus = lens _dassrsStatus (\ s a -> s{_dassrsStatus = a});

-- | The status of the analysis scheme being deleted.
dassrsAnalysisScheme :: Lens' DeleteAnalysisSchemeResponse AnalysisSchemeStatus
dassrsAnalysisScheme = lens _dassrsAnalysisScheme (\ s a -> s{_dassrsAnalysisScheme = a});
