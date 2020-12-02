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
-- Module      : Network.AWS.ServiceCatalog.CreatePortfolioShare
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares the specified portfolio with the specified account.
--
--
module Network.AWS.ServiceCatalog.CreatePortfolioShare
    (
    -- * Creating a Request
      createPortfolioShare
    , CreatePortfolioShare
    -- * Request Lenses
    , cpsAcceptLanguage
    , cpsPortfolioId
    , cpsAccountId

    -- * Destructuring the Response
    , createPortfolioShareResponse
    , CreatePortfolioShareResponse
    -- * Response Lenses
    , cpsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'createPortfolioShare' smart constructor.
data CreatePortfolioShare = CreatePortfolioShare'
  { _cpsAcceptLanguage :: !(Maybe Text)
  , _cpsPortfolioId    :: !Text
  , _cpsAccountId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePortfolioShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'cpsPortfolioId' - The portfolio identifier.
--
-- * 'cpsAccountId' - The AWS account ID.
createPortfolioShare
    :: Text -- ^ 'cpsPortfolioId'
    -> Text -- ^ 'cpsAccountId'
    -> CreatePortfolioShare
createPortfolioShare pPortfolioId_ pAccountId_ =
  CreatePortfolioShare'
    { _cpsAcceptLanguage = Nothing
    , _cpsPortfolioId = pPortfolioId_
    , _cpsAccountId = pAccountId_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
cpsAcceptLanguage :: Lens' CreatePortfolioShare (Maybe Text)
cpsAcceptLanguage = lens _cpsAcceptLanguage (\ s a -> s{_cpsAcceptLanguage = a})

-- | The portfolio identifier.
cpsPortfolioId :: Lens' CreatePortfolioShare Text
cpsPortfolioId = lens _cpsPortfolioId (\ s a -> s{_cpsPortfolioId = a})

-- | The AWS account ID.
cpsAccountId :: Lens' CreatePortfolioShare Text
cpsAccountId = lens _cpsAccountId (\ s a -> s{_cpsAccountId = a})

instance AWSRequest CreatePortfolioShare where
        type Rs CreatePortfolioShare =
             CreatePortfolioShareResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 CreatePortfolioShareResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreatePortfolioShare where

instance NFData CreatePortfolioShare where

instance ToHeaders CreatePortfolioShare where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.CreatePortfolioShare"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePortfolioShare where
        toJSON CreatePortfolioShare'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _cpsAcceptLanguage,
                  Just ("PortfolioId" .= _cpsPortfolioId),
                  Just ("AccountId" .= _cpsAccountId)])

instance ToPath CreatePortfolioShare where
        toPath = const "/"

instance ToQuery CreatePortfolioShare where
        toQuery = const mempty

-- | /See:/ 'createPortfolioShareResponse' smart constructor.
newtype CreatePortfolioShareResponse = CreatePortfolioShareResponse'
  { _cpsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePortfolioShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsrsResponseStatus' - -- | The response status code.
createPortfolioShareResponse
    :: Int -- ^ 'cpsrsResponseStatus'
    -> CreatePortfolioShareResponse
createPortfolioShareResponse pResponseStatus_ =
  CreatePortfolioShareResponse' {_cpsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cpsrsResponseStatus :: Lens' CreatePortfolioShareResponse Int
cpsrsResponseStatus = lens _cpsrsResponseStatus (\ s a -> s{_cpsrsResponseStatus = a})

instance NFData CreatePortfolioShareResponse where
