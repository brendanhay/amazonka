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
-- Module      : Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified principal ARN with the specified portfolio.
--
--
module Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio
    (
    -- * Creating a Request
      associatePrincipalWithPortfolio
    , AssociatePrincipalWithPortfolio
    -- * Request Lenses
    , aAcceptLanguage
    , aPortfolioId
    , aPrincipalARN
    , aPrincipalType

    -- * Destructuring the Response
    , associatePrincipalWithPortfolioResponse
    , AssociatePrincipalWithPortfolioResponse
    -- * Response Lenses
    , apwprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'associatePrincipalWithPortfolio' smart constructor.
data AssociatePrincipalWithPortfolio = AssociatePrincipalWithPortfolio'
  { _aAcceptLanguage :: !(Maybe Text)
  , _aPortfolioId    :: !Text
  , _aPrincipalARN   :: !Text
  , _aPrincipalType  :: !PrincipalType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociatePrincipalWithPortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'aPortfolioId' - The portfolio identifier.
--
-- * 'aPrincipalARN' - The ARN of the principal (IAM user, role, or group).
--
-- * 'aPrincipalType' - The principal type. The supported value is @IAM@ .
associatePrincipalWithPortfolio
    :: Text -- ^ 'aPortfolioId'
    -> Text -- ^ 'aPrincipalARN'
    -> PrincipalType -- ^ 'aPrincipalType'
    -> AssociatePrincipalWithPortfolio
associatePrincipalWithPortfolio pPortfolioId_ pPrincipalARN_ pPrincipalType_ =
  AssociatePrincipalWithPortfolio'
    { _aAcceptLanguage = Nothing
    , _aPortfolioId = pPortfolioId_
    , _aPrincipalARN = pPrincipalARN_
    , _aPrincipalType = pPrincipalType_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
aAcceptLanguage :: Lens' AssociatePrincipalWithPortfolio (Maybe Text)
aAcceptLanguage = lens _aAcceptLanguage (\ s a -> s{_aAcceptLanguage = a})

-- | The portfolio identifier.
aPortfolioId :: Lens' AssociatePrincipalWithPortfolio Text
aPortfolioId = lens _aPortfolioId (\ s a -> s{_aPortfolioId = a})

-- | The ARN of the principal (IAM user, role, or group).
aPrincipalARN :: Lens' AssociatePrincipalWithPortfolio Text
aPrincipalARN = lens _aPrincipalARN (\ s a -> s{_aPrincipalARN = a})

-- | The principal type. The supported value is @IAM@ .
aPrincipalType :: Lens' AssociatePrincipalWithPortfolio PrincipalType
aPrincipalType = lens _aPrincipalType (\ s a -> s{_aPrincipalType = a})

instance AWSRequest AssociatePrincipalWithPortfolio
         where
        type Rs AssociatePrincipalWithPortfolio =
             AssociatePrincipalWithPortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 AssociatePrincipalWithPortfolioResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociatePrincipalWithPortfolio
         where

instance NFData AssociatePrincipalWithPortfolio where

instance ToHeaders AssociatePrincipalWithPortfolio
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.AssociatePrincipalWithPortfolio"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociatePrincipalWithPortfolio where
        toJSON AssociatePrincipalWithPortfolio'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _aAcceptLanguage,
                  Just ("PortfolioId" .= _aPortfolioId),
                  Just ("PrincipalARN" .= _aPrincipalARN),
                  Just ("PrincipalType" .= _aPrincipalType)])

instance ToPath AssociatePrincipalWithPortfolio where
        toPath = const "/"

instance ToQuery AssociatePrincipalWithPortfolio
         where
        toQuery = const mempty

-- | /See:/ 'associatePrincipalWithPortfolioResponse' smart constructor.
newtype AssociatePrincipalWithPortfolioResponse = AssociatePrincipalWithPortfolioResponse'
  { _apwprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociatePrincipalWithPortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apwprsResponseStatus' - -- | The response status code.
associatePrincipalWithPortfolioResponse
    :: Int -- ^ 'apwprsResponseStatus'
    -> AssociatePrincipalWithPortfolioResponse
associatePrincipalWithPortfolioResponse pResponseStatus_ =
  AssociatePrincipalWithPortfolioResponse'
    {_apwprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
apwprsResponseStatus :: Lens' AssociatePrincipalWithPortfolioResponse Int
apwprsResponseStatus = lens _apwprsResponseStatus (\ s a -> s{_apwprsResponseStatus = a})

instance NFData
           AssociatePrincipalWithPortfolioResponse
         where
