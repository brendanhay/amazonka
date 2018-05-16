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
-- Module      : Network.AWS.ServiceCatalog.CreatePortfolio
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a portfolio.
--
--
module Network.AWS.ServiceCatalog.CreatePortfolio
    (
    -- * Creating a Request
      createPortfolio
    , CreatePortfolio
    -- * Request Lenses
    , creAcceptLanguage
    , creDescription
    , creTags
    , creDisplayName
    , creProviderName
    , creIdempotencyToken

    -- * Destructuring the Response
    , createPortfolioResponse
    , CreatePortfolioResponse
    -- * Response Lenses
    , crsPortfolioDetail
    , crsTags
    , crsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'createPortfolio' smart constructor.
data CreatePortfolio = CreatePortfolio'
  { _creAcceptLanguage   :: !(Maybe Text)
  , _creDescription      :: !(Maybe Text)
  , _creTags             :: !(Maybe [Tag])
  , _creDisplayName      :: !Text
  , _creProviderName     :: !Text
  , _creIdempotencyToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'creDescription' - The description of the portfolio.
--
-- * 'creTags' - One or more tags.
--
-- * 'creDisplayName' - The name to use for display purposes.
--
-- * 'creProviderName' - The name of the portfolio provider.
--
-- * 'creIdempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
createPortfolio
    :: Text -- ^ 'creDisplayName'
    -> Text -- ^ 'creProviderName'
    -> Text -- ^ 'creIdempotencyToken'
    -> CreatePortfolio
createPortfolio pDisplayName_ pProviderName_ pIdempotencyToken_ =
  CreatePortfolio'
    { _creAcceptLanguage = Nothing
    , _creDescription = Nothing
    , _creTags = Nothing
    , _creDisplayName = pDisplayName_
    , _creProviderName = pProviderName_
    , _creIdempotencyToken = pIdempotencyToken_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
creAcceptLanguage :: Lens' CreatePortfolio (Maybe Text)
creAcceptLanguage = lens _creAcceptLanguage (\ s a -> s{_creAcceptLanguage = a})

-- | The description of the portfolio.
creDescription :: Lens' CreatePortfolio (Maybe Text)
creDescription = lens _creDescription (\ s a -> s{_creDescription = a})

-- | One or more tags.
creTags :: Lens' CreatePortfolio [Tag]
creTags = lens _creTags (\ s a -> s{_creTags = a}) . _Default . _Coerce

-- | The name to use for display purposes.
creDisplayName :: Lens' CreatePortfolio Text
creDisplayName = lens _creDisplayName (\ s a -> s{_creDisplayName = a})

-- | The name of the portfolio provider.
creProviderName :: Lens' CreatePortfolio Text
creProviderName = lens _creProviderName (\ s a -> s{_creProviderName = a})

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
creIdempotencyToken :: Lens' CreatePortfolio Text
creIdempotencyToken = lens _creIdempotencyToken (\ s a -> s{_creIdempotencyToken = a})

instance AWSRequest CreatePortfolio where
        type Rs CreatePortfolio = CreatePortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 CreatePortfolioResponse' <$>
                   (x .?> "PortfolioDetail") <*>
                     (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable CreatePortfolio where

instance NFData CreatePortfolio where

instance ToHeaders CreatePortfolio where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.CreatePortfolio" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePortfolio where
        toJSON CreatePortfolio'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _creAcceptLanguage,
                  ("Description" .=) <$> _creDescription,
                  ("Tags" .=) <$> _creTags,
                  Just ("DisplayName" .= _creDisplayName),
                  Just ("ProviderName" .= _creProviderName),
                  Just ("IdempotencyToken" .= _creIdempotencyToken)])

instance ToPath CreatePortfolio where
        toPath = const "/"

instance ToQuery CreatePortfolio where
        toQuery = const mempty

-- | /See:/ 'createPortfolioResponse' smart constructor.
data CreatePortfolioResponse = CreatePortfolioResponse'
  { _crsPortfolioDetail :: !(Maybe PortfolioDetail)
  , _crsTags            :: !(Maybe [Tag])
  , _crsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsPortfolioDetail' - Information about the portfolio.
--
-- * 'crsTags' - Information about the tags associated with the portfolio.
--
-- * 'crsResponseStatus' - -- | The response status code.
createPortfolioResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreatePortfolioResponse
createPortfolioResponse pResponseStatus_ =
  CreatePortfolioResponse'
    { _crsPortfolioDetail = Nothing
    , _crsTags = Nothing
    , _crsResponseStatus = pResponseStatus_
    }


-- | Information about the portfolio.
crsPortfolioDetail :: Lens' CreatePortfolioResponse (Maybe PortfolioDetail)
crsPortfolioDetail = lens _crsPortfolioDetail (\ s a -> s{_crsPortfolioDetail = a})

-- | Information about the tags associated with the portfolio.
crsTags :: Lens' CreatePortfolioResponse [Tag]
crsTags = lens _crsTags (\ s a -> s{_crsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
crsResponseStatus :: Lens' CreatePortfolioResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreatePortfolioResponse where
