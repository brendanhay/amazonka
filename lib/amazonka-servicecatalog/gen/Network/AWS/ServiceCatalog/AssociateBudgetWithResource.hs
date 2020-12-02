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
-- Module      : Network.AWS.ServiceCatalog.AssociateBudgetWithResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified budget with the specified resource.
module Network.AWS.ServiceCatalog.AssociateBudgetWithResource
  ( -- * Creating a Request
    associateBudgetWithResource,
    AssociateBudgetWithResource,

    -- * Request Lenses
    abwrBudgetName,
    abwrResourceId,

    -- * Destructuring the Response
    associateBudgetWithResourceResponse,
    AssociateBudgetWithResourceResponse,

    -- * Response Lenses
    abwrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'associateBudgetWithResource' smart constructor.
data AssociateBudgetWithResource = AssociateBudgetWithResource'
  { _abwrBudgetName ::
      !Text,
    _abwrResourceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateBudgetWithResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abwrBudgetName' - The name of the budget you want to associate.
--
-- * 'abwrResourceId' - The resource identifier. Either a portfolio-id or a product-id.
associateBudgetWithResource ::
  -- | 'abwrBudgetName'
  Text ->
  -- | 'abwrResourceId'
  Text ->
  AssociateBudgetWithResource
associateBudgetWithResource pBudgetName_ pResourceId_ =
  AssociateBudgetWithResource'
    { _abwrBudgetName = pBudgetName_,
      _abwrResourceId = pResourceId_
    }

-- | The name of the budget you want to associate.
abwrBudgetName :: Lens' AssociateBudgetWithResource Text
abwrBudgetName = lens _abwrBudgetName (\s a -> s {_abwrBudgetName = a})

-- | The resource identifier. Either a portfolio-id or a product-id.
abwrResourceId :: Lens' AssociateBudgetWithResource Text
abwrResourceId = lens _abwrResourceId (\s a -> s {_abwrResourceId = a})

instance AWSRequest AssociateBudgetWithResource where
  type
    Rs AssociateBudgetWithResource =
      AssociateBudgetWithResourceResponse
  request = postJSON serviceCatalog
  response =
    receiveEmpty
      ( \s h x ->
          AssociateBudgetWithResourceResponse' <$> (pure (fromEnum s))
      )

instance Hashable AssociateBudgetWithResource

instance NFData AssociateBudgetWithResource

instance ToHeaders AssociateBudgetWithResource where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.AssociateBudgetWithResource" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AssociateBudgetWithResource where
  toJSON AssociateBudgetWithResource' {..} =
    object
      ( catMaybes
          [ Just ("BudgetName" .= _abwrBudgetName),
            Just ("ResourceId" .= _abwrResourceId)
          ]
      )

instance ToPath AssociateBudgetWithResource where
  toPath = const "/"

instance ToQuery AssociateBudgetWithResource where
  toQuery = const mempty

-- | /See:/ 'associateBudgetWithResourceResponse' smart constructor.
newtype AssociateBudgetWithResourceResponse = AssociateBudgetWithResourceResponse'
  { _abwrrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateBudgetWithResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abwrrsResponseStatus' - -- | The response status code.
associateBudgetWithResourceResponse ::
  -- | 'abwrrsResponseStatus'
  Int ->
  AssociateBudgetWithResourceResponse
associateBudgetWithResourceResponse pResponseStatus_ =
  AssociateBudgetWithResourceResponse'
    { _abwrrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
abwrrsResponseStatus :: Lens' AssociateBudgetWithResourceResponse Int
abwrrsResponseStatus = lens _abwrrsResponseStatus (\s a -> s {_abwrrsResponseStatus = a})

instance NFData AssociateBudgetWithResourceResponse
