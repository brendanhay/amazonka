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
-- Module      : Network.AWS.ServiceCatalog.AcceptPortfolioShare
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts an offer to share the specified portfolio.
--
--
module Network.AWS.ServiceCatalog.AcceptPortfolioShare
    (
    -- * Creating a Request
      acceptPortfolioShare
    , AcceptPortfolioShare
    -- * Request Lenses
    , apsAcceptLanguage
    , apsPortfolioId

    -- * Destructuring the Response
    , acceptPortfolioShareResponse
    , AcceptPortfolioShareResponse
    -- * Response Lenses
    , apsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'acceptPortfolioShare' smart constructor.
data AcceptPortfolioShare = AcceptPortfolioShare'
  { _apsAcceptLanguage :: !(Maybe Text)
  , _apsPortfolioId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptPortfolioShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apsAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'apsPortfolioId' - The portfolio identifier.
acceptPortfolioShare
    :: Text -- ^ 'apsPortfolioId'
    -> AcceptPortfolioShare
acceptPortfolioShare pPortfolioId_ =
  AcceptPortfolioShare'
    {_apsAcceptLanguage = Nothing, _apsPortfolioId = pPortfolioId_}


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
apsAcceptLanguage :: Lens' AcceptPortfolioShare (Maybe Text)
apsAcceptLanguage = lens _apsAcceptLanguage (\ s a -> s{_apsAcceptLanguage = a})

-- | The portfolio identifier.
apsPortfolioId :: Lens' AcceptPortfolioShare Text
apsPortfolioId = lens _apsPortfolioId (\ s a -> s{_apsPortfolioId = a})

instance AWSRequest AcceptPortfolioShare where
        type Rs AcceptPortfolioShare =
             AcceptPortfolioShareResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 AcceptPortfolioShareResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AcceptPortfolioShare where

instance NFData AcceptPortfolioShare where

instance ToHeaders AcceptPortfolioShare where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.AcceptPortfolioShare"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AcceptPortfolioShare where
        toJSON AcceptPortfolioShare'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _apsAcceptLanguage,
                  Just ("PortfolioId" .= _apsPortfolioId)])

instance ToPath AcceptPortfolioShare where
        toPath = const "/"

instance ToQuery AcceptPortfolioShare where
        toQuery = const mempty

-- | /See:/ 'acceptPortfolioShareResponse' smart constructor.
newtype AcceptPortfolioShareResponse = AcceptPortfolioShareResponse'
  { _apsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptPortfolioShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apsrsResponseStatus' - -- | The response status code.
acceptPortfolioShareResponse
    :: Int -- ^ 'apsrsResponseStatus'
    -> AcceptPortfolioShareResponse
acceptPortfolioShareResponse pResponseStatus_ =
  AcceptPortfolioShareResponse' {_apsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
apsrsResponseStatus :: Lens' AcceptPortfolioShareResponse Int
apsrsResponseStatus = lens _apsrsResponseStatus (\ s a -> s{_apsrsResponseStatus = a})

instance NFData AcceptPortfolioShareResponse where
