{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteIndexField
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes an @IndexField@ from the search domain. For more information,
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
    , diffrqDomainName
    , diffrqIndexFieldName

    -- * Response
    , DeleteIndexFieldResponse
    -- ** Response constructor
    , deleteIndexFieldResponse
    -- ** Response lenses
    , difrsStatus
    , difrsIndexField
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
-- * 'diffrqDomainName'
--
-- * 'diffrqIndexFieldName'
data DeleteIndexField = DeleteIndexField'
    { _diffrqDomainName     :: !Text
    , _diffrqIndexFieldName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteIndexField' smart constructor.
deleteIndexField :: Text -> Text -> DeleteIndexField
deleteIndexField pDomainName pIndexFieldName =
    DeleteIndexField'
    { _diffrqDomainName = pDomainName
    , _diffrqIndexFieldName = pIndexFieldName
    }

-- | FIXME: Undocumented member.
diffrqDomainName :: Lens' DeleteIndexField Text
diffrqDomainName = lens _diffrqDomainName (\ s a -> s{_diffrqDomainName = a});

-- | The name of the index field your want to remove from the domain\'s
-- indexing options.
diffrqIndexFieldName :: Lens' DeleteIndexField Text
diffrqIndexFieldName = lens _diffrqIndexFieldName (\ s a -> s{_diffrqIndexFieldName = a});

instance AWSRequest DeleteIndexField where
        type Sv DeleteIndexField = CloudSearch
        type Rs DeleteIndexField = DeleteIndexFieldResponse
        request = post
        response
          = receiveXMLWrapper "DeleteIndexFieldResult"
              (\ s h x ->
                 DeleteIndexFieldResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "IndexField"))

instance ToHeaders DeleteIndexField where
        toHeaders = const mempty

instance ToPath DeleteIndexField where
        toPath = const "/"

instance ToQuery DeleteIndexField where
        toQuery DeleteIndexField'{..}
          = mconcat
              ["Action" =: ("DeleteIndexField" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _diffrqDomainName,
               "IndexFieldName" =: _diffrqIndexFieldName]

-- | The result of a @DeleteIndexField@ request.
--
-- /See:/ 'deleteIndexFieldResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'difrsStatus'
--
-- * 'difrsIndexField'
data DeleteIndexFieldResponse = DeleteIndexFieldResponse'
    { _difrsStatus     :: !Int
    , _difrsIndexField :: !IndexFieldStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteIndexFieldResponse' smart constructor.
deleteIndexFieldResponse :: Int -> IndexFieldStatus -> DeleteIndexFieldResponse
deleteIndexFieldResponse pStatus pIndexField =
    DeleteIndexFieldResponse'
    { _difrsStatus = pStatus
    , _difrsIndexField = pIndexField
    }

-- | FIXME: Undocumented member.
difrsStatus :: Lens' DeleteIndexFieldResponse Int
difrsStatus = lens _difrsStatus (\ s a -> s{_difrsStatus = a});

-- | The status of the index field being deleted.
difrsIndexField :: Lens' DeleteIndexFieldResponse IndexFieldStatus
difrsIndexField = lens _difrsIndexField (\ s a -> s{_difrsIndexField = a});
