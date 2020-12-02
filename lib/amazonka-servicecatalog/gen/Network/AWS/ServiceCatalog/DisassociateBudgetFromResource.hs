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
-- Module      : Network.AWS.ServiceCatalog.DisassociateBudgetFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified budget from the specified resource.
module Network.AWS.ServiceCatalog.DisassociateBudgetFromResource
  ( -- * Creating a Request
    disassociateBudgetFromResource,
    DisassociateBudgetFromResource,

    -- * Request Lenses
    dbfrBudgetName,
    dbfrResourceId,

    -- * Destructuring the Response
    disassociateBudgetFromResourceResponse,
    DisassociateBudgetFromResourceResponse,

    -- * Response Lenses
    dbfrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'disassociateBudgetFromResource' smart constructor.
data DisassociateBudgetFromResource = DisassociateBudgetFromResource'
  { _dbfrBudgetName ::
      !Text,
    _dbfrResourceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateBudgetFromResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbfrBudgetName' - The name of the budget you want to disassociate.
--
-- * 'dbfrResourceId' - The resource identifier you want to disassociate from. Either a portfolio-id or a product-id.
disassociateBudgetFromResource ::
  -- | 'dbfrBudgetName'
  Text ->
  -- | 'dbfrResourceId'
  Text ->
  DisassociateBudgetFromResource
disassociateBudgetFromResource pBudgetName_ pResourceId_ =
  DisassociateBudgetFromResource'
    { _dbfrBudgetName = pBudgetName_,
      _dbfrResourceId = pResourceId_
    }

-- | The name of the budget you want to disassociate.
dbfrBudgetName :: Lens' DisassociateBudgetFromResource Text
dbfrBudgetName = lens _dbfrBudgetName (\s a -> s {_dbfrBudgetName = a})

-- | The resource identifier you want to disassociate from. Either a portfolio-id or a product-id.
dbfrResourceId :: Lens' DisassociateBudgetFromResource Text
dbfrResourceId = lens _dbfrResourceId (\s a -> s {_dbfrResourceId = a})

instance AWSRequest DisassociateBudgetFromResource where
  type
    Rs DisassociateBudgetFromResource =
      DisassociateBudgetFromResourceResponse
  request = postJSON serviceCatalog
  response =
    receiveEmpty
      ( \s h x ->
          DisassociateBudgetFromResourceResponse' <$> (pure (fromEnum s))
      )

instance Hashable DisassociateBudgetFromResource

instance NFData DisassociateBudgetFromResource

instance ToHeaders DisassociateBudgetFromResource where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.DisassociateBudgetFromResource" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DisassociateBudgetFromResource where
  toJSON DisassociateBudgetFromResource' {..} =
    object
      ( catMaybes
          [ Just ("BudgetName" .= _dbfrBudgetName),
            Just ("ResourceId" .= _dbfrResourceId)
          ]
      )

instance ToPath DisassociateBudgetFromResource where
  toPath = const "/"

instance ToQuery DisassociateBudgetFromResource where
  toQuery = const mempty

-- | /See:/ 'disassociateBudgetFromResourceResponse' smart constructor.
newtype DisassociateBudgetFromResourceResponse = DisassociateBudgetFromResourceResponse'
  { _dbfrrsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DisassociateBudgetFromResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbfrrsResponseStatus' - -- | The response status code.
disassociateBudgetFromResourceResponse ::
  -- | 'dbfrrsResponseStatus'
  Int ->
  DisassociateBudgetFromResourceResponse
disassociateBudgetFromResourceResponse pResponseStatus_ =
  DisassociateBudgetFromResourceResponse'
    { _dbfrrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dbfrrsResponseStatus :: Lens' DisassociateBudgetFromResourceResponse Int
dbfrrsResponseStatus = lens _dbfrrsResponseStatus (\s a -> s {_dbfrrsResponseStatus = a})

instance NFData DisassociateBudgetFromResourceResponse
