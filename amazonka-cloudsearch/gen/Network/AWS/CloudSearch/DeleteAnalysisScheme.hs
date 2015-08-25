{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteAnalysisScheme
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an analysis scheme. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DeleteAnalysisScheme.html AWS API Reference> for DeleteAnalysisScheme.
module Network.AWS.CloudSearch.DeleteAnalysisScheme
    (
    -- * Creating a Request
      deleteAnalysisScheme
    , DeleteAnalysisScheme
    -- * Request Lenses
    , dasDomainName
    , dasAnalysisSchemeName

    -- * Destructuring the Response
    , deleteAnalysisSchemeResponse
    , DeleteAnalysisSchemeResponse
    -- * Response Lenses
    , dasarsStatus
    , dasarsAnalysisScheme
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.CloudSearch.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the 'DeleteAnalysisScheme' operation.
-- Specifies the name of the domain you want to update and the analysis
-- scheme you want to delete.
--
-- /See:/ 'deleteAnalysisScheme' smart constructor.
data DeleteAnalysisScheme = DeleteAnalysisScheme'
    { _dasDomainName         :: !Text
    , _dasAnalysisSchemeName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteAnalysisScheme' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasDomainName'
--
-- * 'dasAnalysisSchemeName'
deleteAnalysisScheme
    :: Text -- ^ 'dasDomainName'
    -> Text -- ^ 'dasAnalysisSchemeName'
    -> DeleteAnalysisScheme
deleteAnalysisScheme pDomainName_ pAnalysisSchemeName_ =
    DeleteAnalysisScheme'
    { _dasDomainName = pDomainName_
    , _dasAnalysisSchemeName = pAnalysisSchemeName_
    }

-- | Undocumented member.
dasDomainName :: Lens' DeleteAnalysisScheme Text
dasDomainName = lens _dasDomainName (\ s a -> s{_dasDomainName = a});

-- | The name of the analysis scheme you want to delete.
dasAnalysisSchemeName :: Lens' DeleteAnalysisScheme Text
dasAnalysisSchemeName = lens _dasAnalysisSchemeName (\ s a -> s{_dasAnalysisSchemeName = a});

instance AWSRequest DeleteAnalysisScheme where
        type Rs DeleteAnalysisScheme =
             DeleteAnalysisSchemeResponse
        request = postQuery cloudSearch
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

-- | The result of a 'DeleteAnalysisScheme' request. Contains the status of
-- the deleted analysis scheme.
--
-- /See:/ 'deleteAnalysisSchemeResponse' smart constructor.
data DeleteAnalysisSchemeResponse = DeleteAnalysisSchemeResponse'
    { _dasarsStatus         :: !Int
    , _dasarsAnalysisScheme :: !AnalysisSchemeStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteAnalysisSchemeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasarsStatus'
--
-- * 'dasarsAnalysisScheme'
deleteAnalysisSchemeResponse
    :: Int -- ^ 'dasarsStatus'
    -> AnalysisSchemeStatus -- ^ 'dasarsAnalysisScheme'
    -> DeleteAnalysisSchemeResponse
deleteAnalysisSchemeResponse pStatus_ pAnalysisScheme_ =
    DeleteAnalysisSchemeResponse'
    { _dasarsStatus = pStatus_
    , _dasarsAnalysisScheme = pAnalysisScheme_
    }

-- | The response status code.
dasarsStatus :: Lens' DeleteAnalysisSchemeResponse Int
dasarsStatus = lens _dasarsStatus (\ s a -> s{_dasarsStatus = a});

-- | The status of the analysis scheme being deleted.
dasarsAnalysisScheme :: Lens' DeleteAnalysisSchemeResponse AnalysisSchemeStatus
dasarsAnalysisScheme = lens _dasarsAnalysisScheme (\ s a -> s{_dasarsAnalysisScheme = a});
