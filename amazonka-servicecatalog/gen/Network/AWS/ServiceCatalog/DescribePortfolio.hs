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
-- Module      : Network.AWS.ServiceCatalog.DescribePortfolio
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves detailed information and any tags associated with the specified portfolio.
--
--
module Network.AWS.ServiceCatalog.DescribePortfolio
    (
    -- * Creating a Request
      describePortfolio
    , DescribePortfolio
    -- * Request Lenses
    , desAcceptLanguage
    , desId

    -- * Destructuring the Response
    , describePortfolioResponse
    , DescribePortfolioResponse
    -- * Response Lenses
    , dprsPortfolioDetail
    , dprsTagOptions
    , dprsTags
    , dprsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describePortfolio' smart constructor.
data DescribePortfolio = DescribePortfolio'
    { _desAcceptLanguage :: !(Maybe Text)
    , _desId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribePortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'desId' - The identifier of the portfolio for which to retrieve information.
describePortfolio
    :: Text -- ^ 'desId'
    -> DescribePortfolio
describePortfolio pId_ =
    DescribePortfolio'
    { _desAcceptLanguage = Nothing
    , _desId = pId_
    }

-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
desAcceptLanguage :: Lens' DescribePortfolio (Maybe Text)
desAcceptLanguage = lens _desAcceptLanguage (\ s a -> s{_desAcceptLanguage = a});

-- | The identifier of the portfolio for which to retrieve information.
desId :: Lens' DescribePortfolio Text
desId = lens _desId (\ s a -> s{_desId = a});

instance AWSRequest DescribePortfolio where
        type Rs DescribePortfolio = DescribePortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribePortfolioResponse' <$>
                   (x .?> "PortfolioDetail") <*>
                     (x .?> "TagOptions" .!@ mempty)
                     <*> (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribePortfolio

instance NFData DescribePortfolio

instance ToHeaders DescribePortfolio where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribePortfolio" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePortfolio where
        toJSON DescribePortfolio'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _desAcceptLanguage,
                  Just ("Id" .= _desId)])

instance ToPath DescribePortfolio where
        toPath = const "/"

instance ToQuery DescribePortfolio where
        toQuery = const mempty

-- | /See:/ 'describePortfolioResponse' smart constructor.
data DescribePortfolioResponse = DescribePortfolioResponse'
    { _dprsPortfolioDetail :: !(Maybe PortfolioDetail)
    , _dprsTagOptions      :: !(Maybe [TagOptionDetail])
    , _dprsTags            :: !(Maybe [Tag])
    , _dprsResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribePortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsPortfolioDetail' - Detailed portfolio information.
--
-- * 'dprsTagOptions' - TagOptions associated with the portfolio.
--
-- * 'dprsTags' - Tags associated with the portfolio.
--
-- * 'dprsResponseStatus' - -- | The response status code.
describePortfolioResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DescribePortfolioResponse
describePortfolioResponse pResponseStatus_ =
    DescribePortfolioResponse'
    { _dprsPortfolioDetail = Nothing
    , _dprsTagOptions = Nothing
    , _dprsTags = Nothing
    , _dprsResponseStatus = pResponseStatus_
    }

-- | Detailed portfolio information.
dprsPortfolioDetail :: Lens' DescribePortfolioResponse (Maybe PortfolioDetail)
dprsPortfolioDetail = lens _dprsPortfolioDetail (\ s a -> s{_dprsPortfolioDetail = a});

-- | TagOptions associated with the portfolio.
dprsTagOptions :: Lens' DescribePortfolioResponse [TagOptionDetail]
dprsTagOptions = lens _dprsTagOptions (\ s a -> s{_dprsTagOptions = a}) . _Default . _Coerce;

-- | Tags associated with the portfolio.
dprsTags :: Lens' DescribePortfolioResponse [Tag]
dprsTags = lens _dprsTags (\ s a -> s{_dprsTags = a}) . _Default . _Coerce;

-- | -- | The response status code.
dprsResponseStatus :: Lens' DescribePortfolioResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a});

instance NFData DescribePortfolioResponse
