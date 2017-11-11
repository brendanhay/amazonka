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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new portfolio.
--
--
module Network.AWS.ServiceCatalog.CreatePortfolio
    (
    -- * Creating a Request
      createPortfolio
    , CreatePortfolio
    -- * Request Lenses
    , cAcceptLanguage
    , cDescription
    , cTags
    , cDisplayName
    , cProviderName
    , cIdempotencyToken

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
  { _cAcceptLanguage   :: {-# NOUNPACK #-}!(Maybe Text)
  , _cDescription      :: {-# NOUNPACK #-}!(Maybe Text)
  , _cTags             :: {-# NOUNPACK #-}!(Maybe [Tag])
  , _cDisplayName      :: {-# NOUNPACK #-}!Text
  , _cProviderName     :: {-# NOUNPACK #-}!Text
  , _cIdempotencyToken :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'cDescription' - The text description of the portfolio.
--
-- * 'cTags' - Tags to associate with the new portfolio.
--
-- * 'cDisplayName' - The name to use for display purposes.
--
-- * 'cProviderName' - The name of the portfolio provider.
--
-- * 'cIdempotencyToken' - A token to disambiguate duplicate requests. You can create multiple resources using the same input in multiple requests, provided that you also specify a different idempotency token for each request.
createPortfolio
    :: Text -- ^ 'cDisplayName'
    -> Text -- ^ 'cProviderName'
    -> Text -- ^ 'cIdempotencyToken'
    -> CreatePortfolio
createPortfolio pDisplayName_ pProviderName_ pIdempotencyToken_ =
  CreatePortfolio'
  { _cAcceptLanguage = Nothing
  , _cDescription = Nothing
  , _cTags = Nothing
  , _cDisplayName = pDisplayName_
  , _cProviderName = pProviderName_
  , _cIdempotencyToken = pIdempotencyToken_
  }


-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
cAcceptLanguage :: Lens' CreatePortfolio (Maybe Text)
cAcceptLanguage = lens _cAcceptLanguage (\ s a -> s{_cAcceptLanguage = a});

-- | The text description of the portfolio.
cDescription :: Lens' CreatePortfolio (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a});

-- | Tags to associate with the new portfolio.
cTags :: Lens' CreatePortfolio [Tag]
cTags = lens _cTags (\ s a -> s{_cTags = a}) . _Default . _Coerce;

-- | The name to use for display purposes.
cDisplayName :: Lens' CreatePortfolio Text
cDisplayName = lens _cDisplayName (\ s a -> s{_cDisplayName = a});

-- | The name of the portfolio provider.
cProviderName :: Lens' CreatePortfolio Text
cProviderName = lens _cProviderName (\ s a -> s{_cProviderName = a});

-- | A token to disambiguate duplicate requests. You can create multiple resources using the same input in multiple requests, provided that you also specify a different idempotency token for each request.
cIdempotencyToken :: Lens' CreatePortfolio Text
cIdempotencyToken = lens _cIdempotencyToken (\ s a -> s{_cIdempotencyToken = a});

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
                 [("AcceptLanguage" .=) <$> _cAcceptLanguage,
                  ("Description" .=) <$> _cDescription,
                  ("Tags" .=) <$> _cTags,
                  Just ("DisplayName" .= _cDisplayName),
                  Just ("ProviderName" .= _cProviderName),
                  Just ("IdempotencyToken" .= _cIdempotencyToken)])

instance ToPath CreatePortfolio where
        toPath = const "/"

instance ToQuery CreatePortfolio where
        toQuery = const mempty

-- | /See:/ 'createPortfolioResponse' smart constructor.
data CreatePortfolioResponse = CreatePortfolioResponse'
  { _crsPortfolioDetail :: {-# NOUNPACK #-}!(Maybe PortfolioDetail)
  , _crsTags            :: {-# NOUNPACK #-}!(Maybe [Tag])
  , _crsResponseStatus  :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsPortfolioDetail' - The resulting detailed portfolio information.
--
-- * 'crsTags' - Tags successfully associated with the new portfolio.
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


-- | The resulting detailed portfolio information.
crsPortfolioDetail :: Lens' CreatePortfolioResponse (Maybe PortfolioDetail)
crsPortfolioDetail = lens _crsPortfolioDetail (\ s a -> s{_crsPortfolioDetail = a});

-- | Tags successfully associated with the new portfolio.
crsTags :: Lens' CreatePortfolioResponse [Tag]
crsTags = lens _crsTags (\ s a -> s{_crsTags = a}) . _Default . _Coerce;

-- | -- | The response status code.
crsResponseStatus :: Lens' CreatePortfolioResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a});

instance NFData CreatePortfolioResponse where
