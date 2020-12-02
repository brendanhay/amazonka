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
-- Module      : Network.AWS.Glue.DeleteClassifier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a classifier from the Data Catalog.
--
--
module Network.AWS.Glue.DeleteClassifier
    (
    -- * Creating a Request
      deleteClassifier
    , DeleteClassifier
    -- * Request Lenses
    , delName

    -- * Destructuring the Response
    , deleteClassifierResponse
    , DeleteClassifierResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteClassifier' smart constructor.
newtype DeleteClassifier = DeleteClassifier'
  { _delName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delName' - Name of the classifier to remove.
deleteClassifier
    :: Text -- ^ 'delName'
    -> DeleteClassifier
deleteClassifier pName_ = DeleteClassifier' {_delName = pName_}


-- | Name of the classifier to remove.
delName :: Lens' DeleteClassifier Text
delName = lens _delName (\ s a -> s{_delName = a})

instance AWSRequest DeleteClassifier where
        type Rs DeleteClassifier = DeleteClassifierResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteClassifierResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteClassifier where

instance NFData DeleteClassifier where

instance ToHeaders DeleteClassifier where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.DeleteClassifier" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteClassifier where
        toJSON DeleteClassifier'{..}
          = object (catMaybes [Just ("Name" .= _delName)])

instance ToPath DeleteClassifier where
        toPath = const "/"

instance ToQuery DeleteClassifier where
        toQuery = const mempty

-- | /See:/ 'deleteClassifierResponse' smart constructor.
newtype DeleteClassifierResponse = DeleteClassifierResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClassifierResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteClassifierResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteClassifierResponse
deleteClassifierResponse pResponseStatus_ =
  DeleteClassifierResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteClassifierResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteClassifierResponse where
