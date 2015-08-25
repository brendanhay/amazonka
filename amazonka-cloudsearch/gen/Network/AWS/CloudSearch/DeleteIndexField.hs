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
-- Module      : Network.AWS.CloudSearch.DeleteIndexField
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an 'IndexField' from the search domain. For more information,
-- see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DeleteIndexField.html AWS API Reference> for DeleteIndexField.
module Network.AWS.CloudSearch.DeleteIndexField
    (
    -- * Creating a Request
      deleteIndexField
    , DeleteIndexField
    -- * Request Lenses
    , difiDomainName
    , difiIndexFieldName

    -- * Destructuring the Response
    , deleteIndexFieldResponse
    , DeleteIndexFieldResponse
    -- * Response Lenses
    , difrsStatus
    , difrsIndexField
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.CloudSearch.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the 'DeleteIndexField' operation.
-- Specifies the name of the domain you want to update and the name of the
-- index field you want to delete.
--
-- /See:/ 'deleteIndexField' smart constructor.
data DeleteIndexField = DeleteIndexField'
    { _difiDomainName     :: !Text
    , _difiIndexFieldName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteIndexField' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'difiDomainName'
--
-- * 'difiIndexFieldName'
deleteIndexField
    :: Text -- ^ 'difiDomainName'
    -> Text -- ^ 'difiIndexFieldName'
    -> DeleteIndexField
deleteIndexField pDomainName_ pIndexFieldName_ =
    DeleteIndexField'
    { _difiDomainName = pDomainName_
    , _difiIndexFieldName = pIndexFieldName_
    }

-- | Undocumented member.
difiDomainName :: Lens' DeleteIndexField Text
difiDomainName = lens _difiDomainName (\ s a -> s{_difiDomainName = a});

-- | The name of the index field your want to remove from the domain\'s
-- indexing options.
difiIndexFieldName :: Lens' DeleteIndexField Text
difiIndexFieldName = lens _difiIndexFieldName (\ s a -> s{_difiIndexFieldName = a});

instance AWSRequest DeleteIndexField where
        type Rs DeleteIndexField = DeleteIndexFieldResponse
        request = postQuery cloudSearch
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
               "DomainName" =: _difiDomainName,
               "IndexFieldName" =: _difiIndexFieldName]

-- | The result of a 'DeleteIndexField' request.
--
-- /See:/ 'deleteIndexFieldResponse' smart constructor.
data DeleteIndexFieldResponse = DeleteIndexFieldResponse'
    { _difrsStatus     :: !Int
    , _difrsIndexField :: !IndexFieldStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteIndexFieldResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'difrsStatus'
--
-- * 'difrsIndexField'
deleteIndexFieldResponse
    :: Int -- ^ 'difrsStatus'
    -> IndexFieldStatus -- ^ 'difrsIndexField'
    -> DeleteIndexFieldResponse
deleteIndexFieldResponse pStatus_ pIndexField_ =
    DeleteIndexFieldResponse'
    { _difrsStatus = pStatus_
    , _difrsIndexField = pIndexField_
    }

-- | The response status code.
difrsStatus :: Lens' DeleteIndexFieldResponse Int
difrsStatus = lens _difrsStatus (\ s a -> s{_difrsStatus = a});

-- | The status of the index field being deleted.
difrsIndexField :: Lens' DeleteIndexFieldResponse IndexFieldStatus
difrsIndexField = lens _difrsIndexField (\ s a -> s{_difrsIndexField = a});
