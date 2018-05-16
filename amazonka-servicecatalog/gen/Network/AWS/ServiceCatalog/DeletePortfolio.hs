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
-- Module      : Network.AWS.ServiceCatalog.DeletePortfolio
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified portfolio.
--
--
-- You cannot delete a portfolio if it was shared with you or if it has associated products, users, constraints, or shared accounts.
--
module Network.AWS.ServiceCatalog.DeletePortfolio
    (
    -- * Creating a Request
      deletePortfolio
    , DeletePortfolio
    -- * Request Lenses
    , ddAcceptLanguage
    , ddId

    -- * Destructuring the Response
    , deletePortfolioResponse
    , DeletePortfolioResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'deletePortfolio' smart constructor.
data DeletePortfolio = DeletePortfolio'
  { _ddAcceptLanguage :: !(Maybe Text)
  , _ddId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'ddId' - The portfolio identifier.
deletePortfolio
    :: Text -- ^ 'ddId'
    -> DeletePortfolio
deletePortfolio pId_ =
  DeletePortfolio' {_ddAcceptLanguage = Nothing, _ddId = pId_}


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
ddAcceptLanguage :: Lens' DeletePortfolio (Maybe Text)
ddAcceptLanguage = lens _ddAcceptLanguage (\ s a -> s{_ddAcceptLanguage = a})

-- | The portfolio identifier.
ddId :: Lens' DeletePortfolio Text
ddId = lens _ddId (\ s a -> s{_ddId = a})

instance AWSRequest DeletePortfolio where
        type Rs DeletePortfolio = DeletePortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DeletePortfolioResponse' <$> (pure (fromEnum s)))

instance Hashable DeletePortfolio where

instance NFData DeletePortfolio where

instance ToHeaders DeletePortfolio where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DeletePortfolio" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePortfolio where
        toJSON DeletePortfolio'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _ddAcceptLanguage,
                  Just ("Id" .= _ddId)])

instance ToPath DeletePortfolio where
        toPath = const "/"

instance ToQuery DeletePortfolio where
        toQuery = const mempty

-- | /See:/ 'deletePortfolioResponse' smart constructor.
newtype DeletePortfolioResponse = DeletePortfolioResponse'
  { _delrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deletePortfolioResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeletePortfolioResponse
deletePortfolioResponse pResponseStatus_ =
  DeletePortfolioResponse' {_delrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeletePortfolioResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeletePortfolioResponse where
