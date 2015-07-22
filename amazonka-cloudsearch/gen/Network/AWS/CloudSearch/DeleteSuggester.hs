{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteSuggester
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a suggester. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DeleteSuggester.html>
module Network.AWS.CloudSearch.DeleteSuggester
    (
    -- * Request
      DeleteSuggester
    -- ** Request constructor
    , deleteSuggester
    -- ** Request lenses
    , dssrqDomainName
    , dssrqSuggesterName

    -- * Response
    , DeleteSuggesterResponse
    -- ** Response constructor
    , deleteSuggesterResponse
    -- ** Response lenses
    , dssrsStatus
    , dssrsSuggester
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DeleteSuggester@ operation.
-- Specifies the name of the domain you want to update and name of the
-- suggester you want to delete.
--
-- /See:/ 'deleteSuggester' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssrqDomainName'
--
-- * 'dssrqSuggesterName'
data DeleteSuggester = DeleteSuggester'
    { _dssrqDomainName    :: !Text
    , _dssrqSuggesterName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSuggester' smart constructor.
deleteSuggester :: Text -> Text -> DeleteSuggester
deleteSuggester pDomainName pSuggesterName =
    DeleteSuggester'
    { _dssrqDomainName = pDomainName
    , _dssrqSuggesterName = pSuggesterName
    }

-- | FIXME: Undocumented member.
dssrqDomainName :: Lens' DeleteSuggester Text
dssrqDomainName = lens _dssrqDomainName (\ s a -> s{_dssrqDomainName = a});

-- | Specifies the name of the suggester you want to delete.
dssrqSuggesterName :: Lens' DeleteSuggester Text
dssrqSuggesterName = lens _dssrqSuggesterName (\ s a -> s{_dssrqSuggesterName = a});

instance AWSRequest DeleteSuggester where
        type Sv DeleteSuggester = CloudSearch
        type Rs DeleteSuggester = DeleteSuggesterResponse
        request = post
        response
          = receiveXMLWrapper "DeleteSuggesterResult"
              (\ s h x ->
                 DeleteSuggesterResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Suggester"))

instance ToHeaders DeleteSuggester where
        toHeaders = const mempty

instance ToPath DeleteSuggester where
        toPath = const "/"

instance ToQuery DeleteSuggester where
        toQuery DeleteSuggester'{..}
          = mconcat
              ["Action" =: ("DeleteSuggester" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _dssrqDomainName,
               "SuggesterName" =: _dssrqSuggesterName]

-- | The result of a @DeleteSuggester@ request. Contains the status of the
-- deleted suggester.
--
-- /See:/ 'deleteSuggesterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssrsStatus'
--
-- * 'dssrsSuggester'
data DeleteSuggesterResponse = DeleteSuggesterResponse'
    { _dssrsStatus    :: !Int
    , _dssrsSuggester :: !SuggesterStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSuggesterResponse' smart constructor.
deleteSuggesterResponse :: Int -> SuggesterStatus -> DeleteSuggesterResponse
deleteSuggesterResponse pStatus pSuggester =
    DeleteSuggesterResponse'
    { _dssrsStatus = pStatus
    , _dssrsSuggester = pSuggester
    }

-- | FIXME: Undocumented member.
dssrsStatus :: Lens' DeleteSuggesterResponse Int
dssrsStatus = lens _dssrsStatus (\ s a -> s{_dssrsStatus = a});

-- | The status of the suggester being deleted.
dssrsSuggester :: Lens' DeleteSuggesterResponse SuggesterStatus
dssrsSuggester = lens _dssrsSuggester (\ s a -> s{_dssrsSuggester = a});
