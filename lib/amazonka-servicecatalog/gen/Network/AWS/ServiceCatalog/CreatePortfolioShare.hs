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
-- Module      : Network.AWS.ServiceCatalog.CreatePortfolioShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares the specified portfolio with the specified account or organization node. Shares to an organization node can only be created by the management account of an organization or by a delegated administrator. You can share portfolios to an organization, an organizational unit, or a specific account.
--
--
-- Note that if a delegated admin is de-registered, they can no longer create portfolio shares.
--
-- @AWSOrganizationsAccess@ must be enabled in order to create a portfolio share to an organization node.
--
-- You can't share a shared resource. This includes portfolios that contain a shared product.
module Network.AWS.ServiceCatalog.CreatePortfolioShare
  ( -- * Creating a Request
    createPortfolioShare,
    CreatePortfolioShare,

    -- * Request Lenses
    cpsAccountId,
    cpsAcceptLanguage,
    cpsOrganizationNode,
    cpsPortfolioId,

    -- * Destructuring the Response
    createPortfolioShareResponse,
    CreatePortfolioShareResponse,

    -- * Response Lenses
    cpsrsPortfolioShareToken,
    cpsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'createPortfolioShare' smart constructor.
data CreatePortfolioShare = CreatePortfolioShare'
  { _cpsAccountId ::
      !(Maybe Text),
    _cpsAcceptLanguage :: !(Maybe Text),
    _cpsOrganizationNode :: !(Maybe OrganizationNode),
    _cpsPortfolioId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePortfolioShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsAccountId' - The AWS account ID. For example, @123456789012@ .
--
-- * 'cpsAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'cpsOrganizationNode' - The organization node to whom you are going to share. If @OrganizationNode@ is passed in, @PortfolioShare@ will be created for the node an ListOrganizationPortfolioAccessd its children (when applies), and a @PortfolioShareToken@ will be returned in the output in order for the administrator to monitor the status of the @PortfolioShare@ creation process.
--
-- * 'cpsPortfolioId' - The portfolio identifier.
createPortfolioShare ::
  -- | 'cpsPortfolioId'
  Text ->
  CreatePortfolioShare
createPortfolioShare pPortfolioId_ =
  CreatePortfolioShare'
    { _cpsAccountId = Nothing,
      _cpsAcceptLanguage = Nothing,
      _cpsOrganizationNode = Nothing,
      _cpsPortfolioId = pPortfolioId_
    }

-- | The AWS account ID. For example, @123456789012@ .
cpsAccountId :: Lens' CreatePortfolioShare (Maybe Text)
cpsAccountId = lens _cpsAccountId (\s a -> s {_cpsAccountId = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
cpsAcceptLanguage :: Lens' CreatePortfolioShare (Maybe Text)
cpsAcceptLanguage = lens _cpsAcceptLanguage (\s a -> s {_cpsAcceptLanguage = a})

-- | The organization node to whom you are going to share. If @OrganizationNode@ is passed in, @PortfolioShare@ will be created for the node an ListOrganizationPortfolioAccessd its children (when applies), and a @PortfolioShareToken@ will be returned in the output in order for the administrator to monitor the status of the @PortfolioShare@ creation process.
cpsOrganizationNode :: Lens' CreatePortfolioShare (Maybe OrganizationNode)
cpsOrganizationNode = lens _cpsOrganizationNode (\s a -> s {_cpsOrganizationNode = a})

-- | The portfolio identifier.
cpsPortfolioId :: Lens' CreatePortfolioShare Text
cpsPortfolioId = lens _cpsPortfolioId (\s a -> s {_cpsPortfolioId = a})

instance AWSRequest CreatePortfolioShare where
  type Rs CreatePortfolioShare = CreatePortfolioShareResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          CreatePortfolioShareResponse'
            <$> (x .?> "PortfolioShareToken") <*> (pure (fromEnum s))
      )

instance Hashable CreatePortfolioShare

instance NFData CreatePortfolioShare

instance ToHeaders CreatePortfolioShare where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWS242ServiceCatalogService.CreatePortfolioShare" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreatePortfolioShare where
  toJSON CreatePortfolioShare' {..} =
    object
      ( catMaybes
          [ ("AccountId" .=) <$> _cpsAccountId,
            ("AcceptLanguage" .=) <$> _cpsAcceptLanguage,
            ("OrganizationNode" .=) <$> _cpsOrganizationNode,
            Just ("PortfolioId" .= _cpsPortfolioId)
          ]
      )

instance ToPath CreatePortfolioShare where
  toPath = const "/"

instance ToQuery CreatePortfolioShare where
  toQuery = const mempty

-- | /See:/ 'createPortfolioShareResponse' smart constructor.
data CreatePortfolioShareResponse = CreatePortfolioShareResponse'
  { _cpsrsPortfolioShareToken ::
      !(Maybe Text),
    _cpsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePortfolioShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsrsPortfolioShareToken' - The portfolio shares a unique identifier that only returns if the portfolio is shared to an organization node.
--
-- * 'cpsrsResponseStatus' - -- | The response status code.
createPortfolioShareResponse ::
  -- | 'cpsrsResponseStatus'
  Int ->
  CreatePortfolioShareResponse
createPortfolioShareResponse pResponseStatus_ =
  CreatePortfolioShareResponse'
    { _cpsrsPortfolioShareToken =
        Nothing,
      _cpsrsResponseStatus = pResponseStatus_
    }

-- | The portfolio shares a unique identifier that only returns if the portfolio is shared to an organization node.
cpsrsPortfolioShareToken :: Lens' CreatePortfolioShareResponse (Maybe Text)
cpsrsPortfolioShareToken = lens _cpsrsPortfolioShareToken (\s a -> s {_cpsrsPortfolioShareToken = a})

-- | -- | The response status code.
cpsrsResponseStatus :: Lens' CreatePortfolioShareResponse Int
cpsrsResponseStatus = lens _cpsrsResponseStatus (\s a -> s {_cpsrsResponseStatus = a})

instance NFData CreatePortfolioShareResponse
