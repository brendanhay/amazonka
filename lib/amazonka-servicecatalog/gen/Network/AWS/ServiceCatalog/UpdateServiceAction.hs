{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a self-service action.
module Network.AWS.ServiceCatalog.UpdateServiceAction
  ( -- * Creating a Request
    updateServiceAction,
    UpdateServiceAction,

    -- * Request Lenses
    usaDefinition,
    usaName,
    usaAcceptLanguage,
    usaDescription,
    usaId,

    -- * Destructuring the Response
    updateServiceActionResponse,
    UpdateServiceActionResponse,

    -- * Response Lenses
    usarsServiceActionDetail,
    usarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'updateServiceAction' smart constructor.
data UpdateServiceAction = UpdateServiceAction'
  { _usaDefinition ::
      !(Maybe (Map ServiceActionDefinitionKey (Text))),
    _usaName :: !(Maybe Text),
    _usaAcceptLanguage :: !(Maybe Text),
    _usaDescription :: !(Maybe Text),
    _usaId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateServiceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usaDefinition' - A map that defines the self-service action.
--
-- * 'usaName' - The self-service action name.
--
-- * 'usaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'usaDescription' - The self-service action description.
--
-- * 'usaId' - The self-service action identifier.
updateServiceAction ::
  -- | 'usaId'
  Text ->
  UpdateServiceAction
updateServiceAction pId_ =
  UpdateServiceAction'
    { _usaDefinition = Nothing,
      _usaName = Nothing,
      _usaAcceptLanguage = Nothing,
      _usaDescription = Nothing,
      _usaId = pId_
    }

-- | A map that defines the self-service action.
usaDefinition :: Lens' UpdateServiceAction (HashMap ServiceActionDefinitionKey (Text))
usaDefinition = lens _usaDefinition (\s a -> s {_usaDefinition = a}) . _Default . _Map

-- | The self-service action name.
usaName :: Lens' UpdateServiceAction (Maybe Text)
usaName = lens _usaName (\s a -> s {_usaName = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
usaAcceptLanguage :: Lens' UpdateServiceAction (Maybe Text)
usaAcceptLanguage = lens _usaAcceptLanguage (\s a -> s {_usaAcceptLanguage = a})

-- | The self-service action description.
usaDescription :: Lens' UpdateServiceAction (Maybe Text)
usaDescription = lens _usaDescription (\s a -> s {_usaDescription = a})

-- | The self-service action identifier.
usaId :: Lens' UpdateServiceAction Text
usaId = lens _usaId (\s a -> s {_usaId = a})

instance AWSRequest UpdateServiceAction where
  type Rs UpdateServiceAction = UpdateServiceActionResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          UpdateServiceActionResponse'
            <$> (x .?> "ServiceActionDetail") <*> (pure (fromEnum s))
      )

instance Hashable UpdateServiceAction

instance NFData UpdateServiceAction

instance ToHeaders UpdateServiceAction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWS242ServiceCatalogService.UpdateServiceAction" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateServiceAction where
  toJSON UpdateServiceAction' {..} =
    object
      ( catMaybes
          [ ("Definition" .=) <$> _usaDefinition,
            ("Name" .=) <$> _usaName,
            ("AcceptLanguage" .=) <$> _usaAcceptLanguage,
            ("Description" .=) <$> _usaDescription,
            Just ("Id" .= _usaId)
          ]
      )

instance ToPath UpdateServiceAction where
  toPath = const "/"

instance ToQuery UpdateServiceAction where
  toQuery = const mempty

-- | /See:/ 'updateServiceActionResponse' smart constructor.
data UpdateServiceActionResponse = UpdateServiceActionResponse'
  { _usarsServiceActionDetail ::
      !(Maybe ServiceActionDetail),
    _usarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateServiceActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usarsServiceActionDetail' - Detailed information about the self-service action.
--
-- * 'usarsResponseStatus' - -- | The response status code.
updateServiceActionResponse ::
  -- | 'usarsResponseStatus'
  Int ->
  UpdateServiceActionResponse
updateServiceActionResponse pResponseStatus_ =
  UpdateServiceActionResponse'
    { _usarsServiceActionDetail = Nothing,
      _usarsResponseStatus = pResponseStatus_
    }

-- | Detailed information about the self-service action.
usarsServiceActionDetail :: Lens' UpdateServiceActionResponse (Maybe ServiceActionDetail)
usarsServiceActionDetail = lens _usarsServiceActionDetail (\s a -> s {_usarsServiceActionDetail = a})

-- | -- | The response status code.
usarsResponseStatus :: Lens' UpdateServiceActionResponse Int
usarsResponseStatus = lens _usarsResponseStatus (\s a -> s {_usarsResponseStatus = a})

instance NFData UpdateServiceActionResponse
