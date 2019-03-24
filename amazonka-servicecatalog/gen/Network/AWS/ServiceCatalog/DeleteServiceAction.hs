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
-- Module      : Network.AWS.ServiceCatalog.DeleteServiceAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a self-service action.
--
--
module Network.AWS.ServiceCatalog.DeleteServiceAction
    (
    -- * Creating a Request
      deleteServiceAction
    , DeleteServiceAction
    -- * Request Lenses
    , dsasAcceptLanguage
    , dsasId

    -- * Destructuring the Response
    , deleteServiceActionResponse
    , DeleteServiceActionResponse
    -- * Response Lenses
    , dsasrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'deleteServiceAction' smart constructor.
data DeleteServiceAction = DeleteServiceAction'
  { _dsasAcceptLanguage :: !(Maybe Text)
  , _dsasId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServiceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsasAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dsasId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
deleteServiceAction
    :: Text -- ^ 'dsasId'
    -> DeleteServiceAction
deleteServiceAction pId_ =
  DeleteServiceAction' {_dsasAcceptLanguage = Nothing, _dsasId = pId_}


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dsasAcceptLanguage :: Lens' DeleteServiceAction (Maybe Text)
dsasAcceptLanguage = lens _dsasAcceptLanguage (\ s a -> s{_dsasAcceptLanguage = a})

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
dsasId :: Lens' DeleteServiceAction Text
dsasId = lens _dsasId (\ s a -> s{_dsasId = a})

instance AWSRequest DeleteServiceAction where
        type Rs DeleteServiceAction =
             DeleteServiceActionResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteServiceActionResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteServiceAction where

instance NFData DeleteServiceAction where

instance ToHeaders DeleteServiceAction where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DeleteServiceAction" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteServiceAction where
        toJSON DeleteServiceAction'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dsasAcceptLanguage,
                  Just ("Id" .= _dsasId)])

instance ToPath DeleteServiceAction where
        toPath = const "/"

instance ToQuery DeleteServiceAction where
        toQuery = const mempty

-- | /See:/ 'deleteServiceActionResponse' smart constructor.
newtype DeleteServiceActionResponse = DeleteServiceActionResponse'
  { _dsasrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServiceActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsasrsResponseStatus' - -- | The response status code.
deleteServiceActionResponse
    :: Int -- ^ 'dsasrsResponseStatus'
    -> DeleteServiceActionResponse
deleteServiceActionResponse pResponseStatus_ =
  DeleteServiceActionResponse' {_dsasrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsasrsResponseStatus :: Lens' DeleteServiceActionResponse Int
dsasrsResponseStatus = lens _dsasrsResponseStatus (\ s a -> s{_dsasrsResponseStatus = a})

instance NFData DeleteServiceActionResponse where
