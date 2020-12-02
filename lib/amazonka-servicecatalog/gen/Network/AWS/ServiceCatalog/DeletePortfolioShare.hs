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
-- Module      : Network.AWS.ServiceCatalog.DeletePortfolioShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops sharing the specified portfolio with the specified account or organization node. Shares to an organization node can only be deleted by the management account of an organization or by a delegated administrator.
--
--
-- Note that if a delegated admin is de-registered, portfolio shares created from that account are removed.
module Network.AWS.ServiceCatalog.DeletePortfolioShare
  ( -- * Creating a Request
    deletePortfolioShare,
    DeletePortfolioShare,

    -- * Request Lenses
    dpsAccountId,
    dpsAcceptLanguage,
    dpsOrganizationNode,
    dpsPortfolioId,

    -- * Destructuring the Response
    deletePortfolioShareResponse,
    DeletePortfolioShareResponse,

    -- * Response Lenses
    dpsrsPortfolioShareToken,
    dpsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'deletePortfolioShare' smart constructor.
data DeletePortfolioShare = DeletePortfolioShare'
  { _dpsAccountId ::
      !(Maybe Text),
    _dpsAcceptLanguage :: !(Maybe Text),
    _dpsOrganizationNode :: !(Maybe OrganizationNode),
    _dpsPortfolioId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePortfolioShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpsAccountId' - The AWS account ID.
--
-- * 'dpsAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dpsOrganizationNode' - The organization node to whom you are going to stop sharing.
--
-- * 'dpsPortfolioId' - The portfolio identifier.
deletePortfolioShare ::
  -- | 'dpsPortfolioId'
  Text ->
  DeletePortfolioShare
deletePortfolioShare pPortfolioId_ =
  DeletePortfolioShare'
    { _dpsAccountId = Nothing,
      _dpsAcceptLanguage = Nothing,
      _dpsOrganizationNode = Nothing,
      _dpsPortfolioId = pPortfolioId_
    }

-- | The AWS account ID.
dpsAccountId :: Lens' DeletePortfolioShare (Maybe Text)
dpsAccountId = lens _dpsAccountId (\s a -> s {_dpsAccountId = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dpsAcceptLanguage :: Lens' DeletePortfolioShare (Maybe Text)
dpsAcceptLanguage = lens _dpsAcceptLanguage (\s a -> s {_dpsAcceptLanguage = a})

-- | The organization node to whom you are going to stop sharing.
dpsOrganizationNode :: Lens' DeletePortfolioShare (Maybe OrganizationNode)
dpsOrganizationNode = lens _dpsOrganizationNode (\s a -> s {_dpsOrganizationNode = a})

-- | The portfolio identifier.
dpsPortfolioId :: Lens' DeletePortfolioShare Text
dpsPortfolioId = lens _dpsPortfolioId (\s a -> s {_dpsPortfolioId = a})

instance AWSRequest DeletePortfolioShare where
  type Rs DeletePortfolioShare = DeletePortfolioShareResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          DeletePortfolioShareResponse'
            <$> (x .?> "PortfolioShareToken") <*> (pure (fromEnum s))
      )

instance Hashable DeletePortfolioShare

instance NFData DeletePortfolioShare

instance ToHeaders DeletePortfolioShare where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWS242ServiceCatalogService.DeletePortfolioShare" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeletePortfolioShare where
  toJSON DeletePortfolioShare' {..} =
    object
      ( catMaybes
          [ ("AccountId" .=) <$> _dpsAccountId,
            ("AcceptLanguage" .=) <$> _dpsAcceptLanguage,
            ("OrganizationNode" .=) <$> _dpsOrganizationNode,
            Just ("PortfolioId" .= _dpsPortfolioId)
          ]
      )

instance ToPath DeletePortfolioShare where
  toPath = const "/"

instance ToQuery DeletePortfolioShare where
  toQuery = const mempty

-- | /See:/ 'deletePortfolioShareResponse' smart constructor.
data DeletePortfolioShareResponse = DeletePortfolioShareResponse'
  { _dpsrsPortfolioShareToken ::
      !(Maybe Text),
    _dpsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePortfolioShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpsrsPortfolioShareToken' - The portfolio share unique identifier. This will only be returned if delete is made to an organization node.
--
-- * 'dpsrsResponseStatus' - -- | The response status code.
deletePortfolioShareResponse ::
  -- | 'dpsrsResponseStatus'
  Int ->
  DeletePortfolioShareResponse
deletePortfolioShareResponse pResponseStatus_ =
  DeletePortfolioShareResponse'
    { _dpsrsPortfolioShareToken =
        Nothing,
      _dpsrsResponseStatus = pResponseStatus_
    }

-- | The portfolio share unique identifier. This will only be returned if delete is made to an organization node.
dpsrsPortfolioShareToken :: Lens' DeletePortfolioShareResponse (Maybe Text)
dpsrsPortfolioShareToken = lens _dpsrsPortfolioShareToken (\s a -> s {_dpsrsPortfolioShareToken = a})

-- | -- | The response status code.
dpsrsResponseStatus :: Lens' DeletePortfolioShareResponse Int
dpsrsResponseStatus = lens _dpsrsResponseStatus (\s a -> s {_dpsrsResponseStatus = a})

instance NFData DeletePortfolioShareResponse
