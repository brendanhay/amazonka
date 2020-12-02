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
-- Module      : Network.AWS.ServiceCatalog.DeletePortfolioShare
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops sharing the specified portfolio with the specified account.
--
--
module Network.AWS.ServiceCatalog.DeletePortfolioShare
    (
    -- * Creating a Request
      deletePortfolioShare
    , DeletePortfolioShare
    -- * Request Lenses
    , dpsAcceptLanguage
    , dpsPortfolioId
    , dpsAccountId

    -- * Destructuring the Response
    , deletePortfolioShareResponse
    , DeletePortfolioShareResponse
    -- * Response Lenses
    , dpsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'deletePortfolioShare' smart constructor.
data DeletePortfolioShare = DeletePortfolioShare'
  { _dpsAcceptLanguage :: !(Maybe Text)
  , _dpsPortfolioId    :: !Text
  , _dpsAccountId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePortfolioShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpsAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dpsPortfolioId' - The portfolio identifier.
--
-- * 'dpsAccountId' - The AWS account ID.
deletePortfolioShare
    :: Text -- ^ 'dpsPortfolioId'
    -> Text -- ^ 'dpsAccountId'
    -> DeletePortfolioShare
deletePortfolioShare pPortfolioId_ pAccountId_ =
  DeletePortfolioShare'
    { _dpsAcceptLanguage = Nothing
    , _dpsPortfolioId = pPortfolioId_
    , _dpsAccountId = pAccountId_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dpsAcceptLanguage :: Lens' DeletePortfolioShare (Maybe Text)
dpsAcceptLanguage = lens _dpsAcceptLanguage (\ s a -> s{_dpsAcceptLanguage = a})

-- | The portfolio identifier.
dpsPortfolioId :: Lens' DeletePortfolioShare Text
dpsPortfolioId = lens _dpsPortfolioId (\ s a -> s{_dpsPortfolioId = a})

-- | The AWS account ID.
dpsAccountId :: Lens' DeletePortfolioShare Text
dpsAccountId = lens _dpsAccountId (\ s a -> s{_dpsAccountId = a})

instance AWSRequest DeletePortfolioShare where
        type Rs DeletePortfolioShare =
             DeletePortfolioShareResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DeletePortfolioShareResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeletePortfolioShare where

instance NFData DeletePortfolioShare where

instance ToHeaders DeletePortfolioShare where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DeletePortfolioShare"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePortfolioShare where
        toJSON DeletePortfolioShare'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dpsAcceptLanguage,
                  Just ("PortfolioId" .= _dpsPortfolioId),
                  Just ("AccountId" .= _dpsAccountId)])

instance ToPath DeletePortfolioShare where
        toPath = const "/"

instance ToQuery DeletePortfolioShare where
        toQuery = const mempty

-- | /See:/ 'deletePortfolioShareResponse' smart constructor.
newtype DeletePortfolioShareResponse = DeletePortfolioShareResponse'
  { _dpsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePortfolioShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpsrsResponseStatus' - -- | The response status code.
deletePortfolioShareResponse
    :: Int -- ^ 'dpsrsResponseStatus'
    -> DeletePortfolioShareResponse
deletePortfolioShareResponse pResponseStatus_ =
  DeletePortfolioShareResponse' {_dpsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dpsrsResponseStatus :: Lens' DeletePortfolioShareResponse Int
dpsrsResponseStatus = lens _dpsrsResponseStatus (\ s a -> s{_dpsrsResponseStatus = a})

instance NFData DeletePortfolioShareResponse where
