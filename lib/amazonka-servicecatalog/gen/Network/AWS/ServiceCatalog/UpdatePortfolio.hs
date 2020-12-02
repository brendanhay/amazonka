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
-- Module      : Network.AWS.ServiceCatalog.UpdatePortfolio
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified portfolio.
--
--
-- You cannot update a product that was shared with you.
--
module Network.AWS.ServiceCatalog.UpdatePortfolio
    (
    -- * Creating a Request
      updatePortfolio
    , UpdatePortfolio
    -- * Request Lenses
    , uRemoveTags
    , uAcceptLanguage
    , uDisplayName
    , uAddTags
    , uDescription
    , uProviderName
    , uId

    -- * Destructuring the Response
    , updatePortfolioResponse
    , UpdatePortfolioResponse
    -- * Response Lenses
    , uprsPortfolioDetail
    , uprsTags
    , uprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'updatePortfolio' smart constructor.
data UpdatePortfolio = UpdatePortfolio'
  { _uRemoveTags     :: !(Maybe [Text])
  , _uAcceptLanguage :: !(Maybe Text)
  , _uDisplayName    :: !(Maybe Text)
  , _uAddTags        :: !(Maybe [Tag])
  , _uDescription    :: !(Maybe Text)
  , _uProviderName   :: !(Maybe Text)
  , _uId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uRemoveTags' - The tags to remove.
--
-- * 'uAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'uDisplayName' - The name to use for display purposes.
--
-- * 'uAddTags' - The tags to add.
--
-- * 'uDescription' - The updated description of the portfolio.
--
-- * 'uProviderName' - The updated name of the portfolio provider.
--
-- * 'uId' - The portfolio identifier.
updatePortfolio
    :: Text -- ^ 'uId'
    -> UpdatePortfolio
updatePortfolio pId_ =
  UpdatePortfolio'
    { _uRemoveTags = Nothing
    , _uAcceptLanguage = Nothing
    , _uDisplayName = Nothing
    , _uAddTags = Nothing
    , _uDescription = Nothing
    , _uProviderName = Nothing
    , _uId = pId_
    }


-- | The tags to remove.
uRemoveTags :: Lens' UpdatePortfolio [Text]
uRemoveTags = lens _uRemoveTags (\ s a -> s{_uRemoveTags = a}) . _Default . _Coerce

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
uAcceptLanguage :: Lens' UpdatePortfolio (Maybe Text)
uAcceptLanguage = lens _uAcceptLanguage (\ s a -> s{_uAcceptLanguage = a})

-- | The name to use for display purposes.
uDisplayName :: Lens' UpdatePortfolio (Maybe Text)
uDisplayName = lens _uDisplayName (\ s a -> s{_uDisplayName = a})

-- | The tags to add.
uAddTags :: Lens' UpdatePortfolio [Tag]
uAddTags = lens _uAddTags (\ s a -> s{_uAddTags = a}) . _Default . _Coerce

-- | The updated description of the portfolio.
uDescription :: Lens' UpdatePortfolio (Maybe Text)
uDescription = lens _uDescription (\ s a -> s{_uDescription = a})

-- | The updated name of the portfolio provider.
uProviderName :: Lens' UpdatePortfolio (Maybe Text)
uProviderName = lens _uProviderName (\ s a -> s{_uProviderName = a})

-- | The portfolio identifier.
uId :: Lens' UpdatePortfolio Text
uId = lens _uId (\ s a -> s{_uId = a})

instance AWSRequest UpdatePortfolio where
        type Rs UpdatePortfolio = UpdatePortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 UpdatePortfolioResponse' <$>
                   (x .?> "PortfolioDetail") <*>
                     (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable UpdatePortfolio where

instance NFData UpdatePortfolio where

instance ToHeaders UpdatePortfolio where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.UpdatePortfolio" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdatePortfolio where
        toJSON UpdatePortfolio'{..}
          = object
              (catMaybes
                 [("RemoveTags" .=) <$> _uRemoveTags,
                  ("AcceptLanguage" .=) <$> _uAcceptLanguage,
                  ("DisplayName" .=) <$> _uDisplayName,
                  ("AddTags" .=) <$> _uAddTags,
                  ("Description" .=) <$> _uDescription,
                  ("ProviderName" .=) <$> _uProviderName,
                  Just ("Id" .= _uId)])

instance ToPath UpdatePortfolio where
        toPath = const "/"

instance ToQuery UpdatePortfolio where
        toQuery = const mempty

-- | /See:/ 'updatePortfolioResponse' smart constructor.
data UpdatePortfolioResponse = UpdatePortfolioResponse'
  { _uprsPortfolioDetail :: !(Maybe PortfolioDetail)
  , _uprsTags            :: !(Maybe [Tag])
  , _uprsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprsPortfolioDetail' - Information about the portfolio.
--
-- * 'uprsTags' - Information about the tags associated with the portfolio.
--
-- * 'uprsResponseStatus' - -- | The response status code.
updatePortfolioResponse
    :: Int -- ^ 'uprsResponseStatus'
    -> UpdatePortfolioResponse
updatePortfolioResponse pResponseStatus_ =
  UpdatePortfolioResponse'
    { _uprsPortfolioDetail = Nothing
    , _uprsTags = Nothing
    , _uprsResponseStatus = pResponseStatus_
    }


-- | Information about the portfolio.
uprsPortfolioDetail :: Lens' UpdatePortfolioResponse (Maybe PortfolioDetail)
uprsPortfolioDetail = lens _uprsPortfolioDetail (\ s a -> s{_uprsPortfolioDetail = a})

-- | Information about the tags associated with the portfolio.
uprsTags :: Lens' UpdatePortfolioResponse [Tag]
uprsTags = lens _uprsTags (\ s a -> s{_uprsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
uprsResponseStatus :: Lens' UpdatePortfolioResponse Int
uprsResponseStatus = lens _uprsResponseStatus (\ s a -> s{_uprsResponseStatus = a})

instance NFData UpdatePortfolioResponse where
