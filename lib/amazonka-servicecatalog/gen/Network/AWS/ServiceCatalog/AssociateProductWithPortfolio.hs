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
-- Module      : Network.AWS.ServiceCatalog.AssociateProductWithPortfolio
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified product with the specified portfolio.
--
--
module Network.AWS.ServiceCatalog.AssociateProductWithPortfolio
    (
    -- * Creating a Request
      associateProductWithPortfolio
    , AssociateProductWithPortfolio
    -- * Request Lenses
    , apwpSourcePortfolioId
    , apwpAcceptLanguage
    , apwpProductId
    , apwpPortfolioId

    -- * Destructuring the Response
    , associateProductWithPortfolioResponse
    , AssociateProductWithPortfolioResponse
    -- * Response Lenses
    , arsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'associateProductWithPortfolio' smart constructor.
data AssociateProductWithPortfolio = AssociateProductWithPortfolio'
  { _apwpSourcePortfolioId :: !(Maybe Text)
  , _apwpAcceptLanguage    :: !(Maybe Text)
  , _apwpProductId         :: !Text
  , _apwpPortfolioId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateProductWithPortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apwpSourcePortfolioId' - The identifier of the source portfolio.
--
-- * 'apwpAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'apwpProductId' - The product identifier.
--
-- * 'apwpPortfolioId' - The portfolio identifier.
associateProductWithPortfolio
    :: Text -- ^ 'apwpProductId'
    -> Text -- ^ 'apwpPortfolioId'
    -> AssociateProductWithPortfolio
associateProductWithPortfolio pProductId_ pPortfolioId_ =
  AssociateProductWithPortfolio'
    { _apwpSourcePortfolioId = Nothing
    , _apwpAcceptLanguage = Nothing
    , _apwpProductId = pProductId_
    , _apwpPortfolioId = pPortfolioId_
    }


-- | The identifier of the source portfolio.
apwpSourcePortfolioId :: Lens' AssociateProductWithPortfolio (Maybe Text)
apwpSourcePortfolioId = lens _apwpSourcePortfolioId (\ s a -> s{_apwpSourcePortfolioId = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
apwpAcceptLanguage :: Lens' AssociateProductWithPortfolio (Maybe Text)
apwpAcceptLanguage = lens _apwpAcceptLanguage (\ s a -> s{_apwpAcceptLanguage = a})

-- | The product identifier.
apwpProductId :: Lens' AssociateProductWithPortfolio Text
apwpProductId = lens _apwpProductId (\ s a -> s{_apwpProductId = a})

-- | The portfolio identifier.
apwpPortfolioId :: Lens' AssociateProductWithPortfolio Text
apwpPortfolioId = lens _apwpPortfolioId (\ s a -> s{_apwpPortfolioId = a})

instance AWSRequest AssociateProductWithPortfolio
         where
        type Rs AssociateProductWithPortfolio =
             AssociateProductWithPortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateProductWithPortfolioResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateProductWithPortfolio where

instance NFData AssociateProductWithPortfolio where

instance ToHeaders AssociateProductWithPortfolio
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.AssociateProductWithPortfolio"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateProductWithPortfolio where
        toJSON AssociateProductWithPortfolio'{..}
          = object
              (catMaybes
                 [("SourcePortfolioId" .=) <$> _apwpSourcePortfolioId,
                  ("AcceptLanguage" .=) <$> _apwpAcceptLanguage,
                  Just ("ProductId" .= _apwpProductId),
                  Just ("PortfolioId" .= _apwpPortfolioId)])

instance ToPath AssociateProductWithPortfolio where
        toPath = const "/"

instance ToQuery AssociateProductWithPortfolio where
        toQuery = const mempty

-- | /See:/ 'associateProductWithPortfolioResponse' smart constructor.
newtype AssociateProductWithPortfolioResponse = AssociateProductWithPortfolioResponse'
  { _arsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateProductWithPortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arsResponseStatus' - -- | The response status code.
associateProductWithPortfolioResponse
    :: Int -- ^ 'arsResponseStatus'
    -> AssociateProductWithPortfolioResponse
associateProductWithPortfolioResponse pResponseStatus_ =
  AssociateProductWithPortfolioResponse' {_arsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
arsResponseStatus :: Lens' AssociateProductWithPortfolioResponse Int
arsResponseStatus = lens _arsResponseStatus (\ s a -> s{_arsResponseStatus = a})

instance NFData AssociateProductWithPortfolioResponse
         where
