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
-- Module      : Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified product from the specified portfolio.
--
--
module Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio
    (
    -- * Creating a Request
      disassociateProductFromPortfolio
    , DisassociateProductFromPortfolio
    -- * Request Lenses
    , dpfpAcceptLanguage
    , dpfpProductId
    , dpfpPortfolioId

    -- * Destructuring the Response
    , disassociateProductFromPortfolioResponse
    , DisassociateProductFromPortfolioResponse
    -- * Response Lenses
    , disrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'disassociateProductFromPortfolio' smart constructor.
data DisassociateProductFromPortfolio = DisassociateProductFromPortfolio'
  { _dpfpAcceptLanguage :: !(Maybe Text)
  , _dpfpProductId      :: !Text
  , _dpfpPortfolioId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateProductFromPortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpfpAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dpfpProductId' - The product identifier.
--
-- * 'dpfpPortfolioId' - The portfolio identifier.
disassociateProductFromPortfolio
    :: Text -- ^ 'dpfpProductId'
    -> Text -- ^ 'dpfpPortfolioId'
    -> DisassociateProductFromPortfolio
disassociateProductFromPortfolio pProductId_ pPortfolioId_ =
  DisassociateProductFromPortfolio'
    { _dpfpAcceptLanguage = Nothing
    , _dpfpProductId = pProductId_
    , _dpfpPortfolioId = pPortfolioId_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dpfpAcceptLanguage :: Lens' DisassociateProductFromPortfolio (Maybe Text)
dpfpAcceptLanguage = lens _dpfpAcceptLanguage (\ s a -> s{_dpfpAcceptLanguage = a})

-- | The product identifier.
dpfpProductId :: Lens' DisassociateProductFromPortfolio Text
dpfpProductId = lens _dpfpProductId (\ s a -> s{_dpfpProductId = a})

-- | The portfolio identifier.
dpfpPortfolioId :: Lens' DisassociateProductFromPortfolio Text
dpfpPortfolioId = lens _dpfpPortfolioId (\ s a -> s{_dpfpPortfolioId = a})

instance AWSRequest DisassociateProductFromPortfolio
         where
        type Rs DisassociateProductFromPortfolio =
             DisassociateProductFromPortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateProductFromPortfolioResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateProductFromPortfolio
         where

instance NFData DisassociateProductFromPortfolio
         where

instance ToHeaders DisassociateProductFromPortfolio
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DisassociateProductFromPortfolio"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateProductFromPortfolio
         where
        toJSON DisassociateProductFromPortfolio'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dpfpAcceptLanguage,
                  Just ("ProductId" .= _dpfpProductId),
                  Just ("PortfolioId" .= _dpfpPortfolioId)])

instance ToPath DisassociateProductFromPortfolio
         where
        toPath = const "/"

instance ToQuery DisassociateProductFromPortfolio
         where
        toQuery = const mempty

-- | /See:/ 'disassociateProductFromPortfolioResponse' smart constructor.
newtype DisassociateProductFromPortfolioResponse = DisassociateProductFromPortfolioResponse'
  { _disrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateProductFromPortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsResponseStatus' - -- | The response status code.
disassociateProductFromPortfolioResponse
    :: Int -- ^ 'disrsResponseStatus'
    -> DisassociateProductFromPortfolioResponse
disassociateProductFromPortfolioResponse pResponseStatus_ =
  DisassociateProductFromPortfolioResponse'
    {_disrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
disrsResponseStatus :: Lens' DisassociateProductFromPortfolioResponse Int
disrsResponseStatus = lens _disrsResponseStatus (\ s a -> s{_disrsResponseStatus = a})

instance NFData
           DisassociateProductFromPortfolioResponse
         where
