{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CloudSearch.DeleteSuggester
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a suggester. For more information, see
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
    , deleDomainName
    , deleSuggesterName

    -- * Response
    , DeleteSuggesterResponse
    -- ** Response constructor
    , deleteSuggesterResponse
    -- ** Response lenses
    , deleStatus
    , deleSuggester
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
-- * 'deleDomainName'
--
-- * 'deleSuggesterName'
data DeleteSuggester = DeleteSuggester'
    { _deleDomainName    :: !Text
    , _deleSuggesterName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSuggester' smart constructor.
deleteSuggester :: Text -> Text -> DeleteSuggester
deleteSuggester pDomainName pSuggesterName =
    DeleteSuggester'
    { _deleDomainName = pDomainName
    , _deleSuggesterName = pSuggesterName
    }

-- | FIXME: Undocumented member.
deleDomainName :: Lens' DeleteSuggester Text
deleDomainName = lens _deleDomainName (\ s a -> s{_deleDomainName = a});

-- | Specifies the name of the suggester you want to delete.
deleSuggesterName :: Lens' DeleteSuggester Text
deleSuggesterName = lens _deleSuggesterName (\ s a -> s{_deleSuggesterName = a});

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
               "DomainName" =: _deleDomainName,
               "SuggesterName" =: _deleSuggesterName]

-- | The result of a @DeleteSuggester@ request. Contains the status of the
-- deleted suggester.
--
-- /See:/ 'deleteSuggesterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deleStatus'
--
-- * 'deleSuggester'
data DeleteSuggesterResponse = DeleteSuggesterResponse'
    { _deleStatus    :: !Int
    , _deleSuggester :: !SuggesterStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSuggesterResponse' smart constructor.
deleteSuggesterResponse :: Int -> SuggesterStatus -> DeleteSuggesterResponse
deleteSuggesterResponse pStatus pSuggester =
    DeleteSuggesterResponse'
    { _deleStatus = pStatus
    , _deleSuggester = pSuggester
    }

-- | FIXME: Undocumented member.
deleStatus :: Lens' DeleteSuggesterResponse Int
deleStatus = lens _deleStatus (\ s a -> s{_deleStatus = a});

-- | The status of the suggester being deleted.
deleSuggester :: Lens' DeleteSuggesterResponse SuggesterStatus
deleSuggester = lens _deleSuggester (\ s a -> s{_deleSuggester = a});
