{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.DeleteIndexField
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

-- | Removes an @IndexField@ from the search domain. For more information,
-- see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DeleteIndexField.html>
module Network.AWS.CloudSearch.DeleteIndexField
    (
    -- * Request
      DeleteIndexField
    -- ** Request constructor
    , deleteIndexField
    -- ** Request lenses
    , dif1DomainName
    , dif1IndexFieldName

    -- * Response
    , DeleteIndexFieldResponse
    -- ** Response constructor
    , deleteIndexFieldResponse
    -- ** Response lenses
    , difrStatus
    , difrIndexField
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DeleteIndexField@ operation.
-- Specifies the name of the domain you want to update and the name of the
-- index field you want to delete.
--
-- /See:/ 'deleteIndexField' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dif1DomainName'
--
-- * 'dif1IndexFieldName'
data DeleteIndexField = DeleteIndexField'
    { _dif1DomainName     :: !Text
    , _dif1IndexFieldName :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeleteIndexField' smart constructor.
deleteIndexField :: Text -> Text -> DeleteIndexField
deleteIndexField pDomainName pIndexFieldName =
    DeleteIndexField'
    { _dif1DomainName = pDomainName
    , _dif1IndexFieldName = pIndexFieldName
    }

-- | FIXME: Undocumented member.
dif1DomainName :: Lens' DeleteIndexField Text
dif1DomainName = lens _dif1DomainName (\ s a -> s{_dif1DomainName = a});

-- | The name of the index field your want to remove from the domain\'s
-- indexing options.
dif1IndexFieldName :: Lens' DeleteIndexField Text
dif1IndexFieldName = lens _dif1IndexFieldName (\ s a -> s{_dif1IndexFieldName = a});

instance AWSRequest DeleteIndexField where
        type Sv DeleteIndexField = CloudSearch
        type Rs DeleteIndexField = DeleteIndexFieldResponse
        request = post
        response
          = receiveXMLWrapper "DeleteIndexFieldResult"
              (\ s h x ->
                 DeleteIndexFieldResponse' <$>
                   (pure s) <*> (x .@ "IndexField"))

instance ToHeaders DeleteIndexField where
        toHeaders = const mempty

instance ToPath DeleteIndexField where
        toPath = const "/"

instance ToQuery DeleteIndexField where
        toQuery DeleteIndexField'{..}
          = mconcat
              ["Action" =: ("DeleteIndexField" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _dif1DomainName,
               "IndexFieldName" =: _dif1IndexFieldName]

-- | The result of a @DeleteIndexField@ request.
--
-- /See:/ 'deleteIndexFieldResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'difrStatus'
--
-- * 'difrIndexField'
data DeleteIndexFieldResponse = DeleteIndexFieldResponse'
    { _difrStatus     :: !Status
    , _difrIndexField :: !IndexFieldStatus
    } deriving (Eq,Show)

-- | 'DeleteIndexFieldResponse' smart constructor.
deleteIndexFieldResponse :: Status -> IndexFieldStatus -> DeleteIndexFieldResponse
deleteIndexFieldResponse pStatus pIndexField =
    DeleteIndexFieldResponse'
    { _difrStatus = pStatus
    , _difrIndexField = pIndexField
    }

-- | FIXME: Undocumented member.
difrStatus :: Lens' DeleteIndexFieldResponse Status
difrStatus = lens _difrStatus (\ s a -> s{_difrStatus = a});

-- | The status of the index field being deleted.
difrIndexField :: Lens' DeleteIndexFieldResponse IndexFieldStatus
difrIndexField = lens _difrIndexField (\ s a -> s{_difrIndexField = a});
