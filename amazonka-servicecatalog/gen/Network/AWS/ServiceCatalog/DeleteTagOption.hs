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
-- Module      : Network.AWS.ServiceCatalog.DeleteTagOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified TagOption.
--
--
-- You cannot delete a TagOption if it is associated with a product or portfolio.
--
module Network.AWS.ServiceCatalog.DeleteTagOption
    (
    -- * Creating a Request
      deleteTagOption
    , DeleteTagOption
    -- * Request Lenses
    , delId

    -- * Destructuring the Response
    , deleteTagOptionResponse
    , DeleteTagOptionResponse
    -- * Response Lenses
    , dtotrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'deleteTagOption' smart constructor.
newtype DeleteTagOption = DeleteTagOption'
  { _delId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTagOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delId' - The TagOption identifier.
deleteTagOption
    :: Text -- ^ 'delId'
    -> DeleteTagOption
deleteTagOption pId_ = DeleteTagOption' {_delId = pId_}


-- | The TagOption identifier.
delId :: Lens' DeleteTagOption Text
delId = lens _delId (\ s a -> s{_delId = a})

instance AWSRequest DeleteTagOption where
        type Rs DeleteTagOption = DeleteTagOptionResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteTagOptionResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteTagOption where

instance NFData DeleteTagOption where

instance ToHeaders DeleteTagOption where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DeleteTagOption" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTagOption where
        toJSON DeleteTagOption'{..}
          = object (catMaybes [Just ("Id" .= _delId)])

instance ToPath DeleteTagOption where
        toPath = const "/"

instance ToQuery DeleteTagOption where
        toQuery = const mempty

-- | /See:/ 'deleteTagOptionResponse' smart constructor.
newtype DeleteTagOptionResponse = DeleteTagOptionResponse'
  { _dtotrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTagOptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtotrsResponseStatus' - -- | The response status code.
deleteTagOptionResponse
    :: Int -- ^ 'dtotrsResponseStatus'
    -> DeleteTagOptionResponse
deleteTagOptionResponse pResponseStatus_ =
  DeleteTagOptionResponse' {_dtotrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dtotrsResponseStatus :: Lens' DeleteTagOptionResponse Int
dtotrsResponseStatus = lens _dtotrsResponseStatus (\ s a -> s{_dtotrsResponseStatus = a})

instance NFData DeleteTagOptionResponse where
