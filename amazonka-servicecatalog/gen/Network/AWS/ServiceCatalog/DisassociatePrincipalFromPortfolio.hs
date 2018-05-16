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
-- Module      : Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a previously associated principal ARN from a specified portfolio.
--
--
module Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio
    (
    -- * Creating a Request
      disassociatePrincipalFromPortfolio
    , DisassociatePrincipalFromPortfolio
    -- * Request Lenses
    , disAcceptLanguage
    , disPortfolioId
    , disPrincipalARN

    -- * Destructuring the Response
    , disassociatePrincipalFromPortfolioResponse
    , DisassociatePrincipalFromPortfolioResponse
    -- * Response Lenses
    , dpfprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'disassociatePrincipalFromPortfolio' smart constructor.
data DisassociatePrincipalFromPortfolio = DisassociatePrincipalFromPortfolio'
  { _disAcceptLanguage :: !(Maybe Text)
  , _disPortfolioId    :: !Text
  , _disPrincipalARN   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociatePrincipalFromPortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'disPortfolioId' - The portfolio identifier.
--
-- * 'disPrincipalARN' - The ARN of the principal (IAM user, role, or group).
disassociatePrincipalFromPortfolio
    :: Text -- ^ 'disPortfolioId'
    -> Text -- ^ 'disPrincipalARN'
    -> DisassociatePrincipalFromPortfolio
disassociatePrincipalFromPortfolio pPortfolioId_ pPrincipalARN_ =
  DisassociatePrincipalFromPortfolio'
    { _disAcceptLanguage = Nothing
    , _disPortfolioId = pPortfolioId_
    , _disPrincipalARN = pPrincipalARN_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
disAcceptLanguage :: Lens' DisassociatePrincipalFromPortfolio (Maybe Text)
disAcceptLanguage = lens _disAcceptLanguage (\ s a -> s{_disAcceptLanguage = a})

-- | The portfolio identifier.
disPortfolioId :: Lens' DisassociatePrincipalFromPortfolio Text
disPortfolioId = lens _disPortfolioId (\ s a -> s{_disPortfolioId = a})

-- | The ARN of the principal (IAM user, role, or group).
disPrincipalARN :: Lens' DisassociatePrincipalFromPortfolio Text
disPrincipalARN = lens _disPrincipalARN (\ s a -> s{_disPrincipalARN = a})

instance AWSRequest
           DisassociatePrincipalFromPortfolio
         where
        type Rs DisassociatePrincipalFromPortfolio =
             DisassociatePrincipalFromPortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociatePrincipalFromPortfolioResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociatePrincipalFromPortfolio
         where

instance NFData DisassociatePrincipalFromPortfolio
         where

instance ToHeaders DisassociatePrincipalFromPortfolio
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DisassociatePrincipalFromPortfolio"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociatePrincipalFromPortfolio
         where
        toJSON DisassociatePrincipalFromPortfolio'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _disAcceptLanguage,
                  Just ("PortfolioId" .= _disPortfolioId),
                  Just ("PrincipalARN" .= _disPrincipalARN)])

instance ToPath DisassociatePrincipalFromPortfolio
         where
        toPath = const "/"

instance ToQuery DisassociatePrincipalFromPortfolio
         where
        toQuery = const mempty

-- | /See:/ 'disassociatePrincipalFromPortfolioResponse' smart constructor.
newtype DisassociatePrincipalFromPortfolioResponse = DisassociatePrincipalFromPortfolioResponse'
  { _dpfprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociatePrincipalFromPortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpfprsResponseStatus' - -- | The response status code.
disassociatePrincipalFromPortfolioResponse
    :: Int -- ^ 'dpfprsResponseStatus'
    -> DisassociatePrincipalFromPortfolioResponse
disassociatePrincipalFromPortfolioResponse pResponseStatus_ =
  DisassociatePrincipalFromPortfolioResponse'
    {_dpfprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dpfprsResponseStatus :: Lens' DisassociatePrincipalFromPortfolioResponse Int
dpfprsResponseStatus = lens _dpfprsResponseStatus (\ s a -> s{_dpfprsResponseStatus = a})

instance NFData
           DisassociatePrincipalFromPortfolioResponse
         where
