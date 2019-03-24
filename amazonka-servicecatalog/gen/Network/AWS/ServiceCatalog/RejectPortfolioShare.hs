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
-- Module      : Network.AWS.ServiceCatalog.RejectPortfolioShare
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects an offer to share the specified portfolio.
--
--
module Network.AWS.ServiceCatalog.RejectPortfolioShare
    (
    -- * Creating a Request
      rejectPortfolioShare
    , RejectPortfolioShare
    -- * Request Lenses
    , rpsPortfolioShareType
    , rpsAcceptLanguage
    , rpsPortfolioId

    -- * Destructuring the Response
    , rejectPortfolioShareResponse
    , RejectPortfolioShareResponse
    -- * Response Lenses
    , rpsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'rejectPortfolioShare' smart constructor.
data RejectPortfolioShare = RejectPortfolioShare'
  { _rpsPortfolioShareType :: !(Maybe PortfolioShareType)
  , _rpsAcceptLanguage     :: !(Maybe Text)
  , _rpsPortfolioId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RejectPortfolioShare' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpsPortfolioShareType' - The type of shared portfolios to reject. The default is to reject imported portfolios.     * @AWS_ORGANIZATIONS@ - Reject portfolios shared by the master account of your organization.     * @IMPORTED@ - Reject imported portfolios.     * @AWS_SERVICECATALOG@ - Not supported. (Throws ResourceNotFoundException.) For example, @aws servicecatalog reject-portfolio-share --portfolio-id "port-2qwzkwxt3y5fk" --portfolio-share-type AWS_ORGANIZATIONS@
--
-- * 'rpsAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'rpsPortfolioId' - The portfolio identifier.
rejectPortfolioShare
    :: Text -- ^ 'rpsPortfolioId'
    -> RejectPortfolioShare
rejectPortfolioShare pPortfolioId_ =
  RejectPortfolioShare'
    { _rpsPortfolioShareType = Nothing
    , _rpsAcceptLanguage = Nothing
    , _rpsPortfolioId = pPortfolioId_
    }


-- | The type of shared portfolios to reject. The default is to reject imported portfolios.     * @AWS_ORGANIZATIONS@ - Reject portfolios shared by the master account of your organization.     * @IMPORTED@ - Reject imported portfolios.     * @AWS_SERVICECATALOG@ - Not supported. (Throws ResourceNotFoundException.) For example, @aws servicecatalog reject-portfolio-share --portfolio-id "port-2qwzkwxt3y5fk" --portfolio-share-type AWS_ORGANIZATIONS@
rpsPortfolioShareType :: Lens' RejectPortfolioShare (Maybe PortfolioShareType)
rpsPortfolioShareType = lens _rpsPortfolioShareType (\ s a -> s{_rpsPortfolioShareType = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
rpsAcceptLanguage :: Lens' RejectPortfolioShare (Maybe Text)
rpsAcceptLanguage = lens _rpsAcceptLanguage (\ s a -> s{_rpsAcceptLanguage = a})

-- | The portfolio identifier.
rpsPortfolioId :: Lens' RejectPortfolioShare Text
rpsPortfolioId = lens _rpsPortfolioId (\ s a -> s{_rpsPortfolioId = a})

instance AWSRequest RejectPortfolioShare where
        type Rs RejectPortfolioShare =
             RejectPortfolioShareResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 RejectPortfolioShareResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RejectPortfolioShare where

instance NFData RejectPortfolioShare where

instance ToHeaders RejectPortfolioShare where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.RejectPortfolioShare"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RejectPortfolioShare where
        toJSON RejectPortfolioShare'{..}
          = object
              (catMaybes
                 [("PortfolioShareType" .=) <$>
                    _rpsPortfolioShareType,
                  ("AcceptLanguage" .=) <$> _rpsAcceptLanguage,
                  Just ("PortfolioId" .= _rpsPortfolioId)])

instance ToPath RejectPortfolioShare where
        toPath = const "/"

instance ToQuery RejectPortfolioShare where
        toQuery = const mempty

-- | /See:/ 'rejectPortfolioShareResponse' smart constructor.
newtype RejectPortfolioShareResponse = RejectPortfolioShareResponse'
  { _rpsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RejectPortfolioShareResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpsrsResponseStatus' - -- | The response status code.
rejectPortfolioShareResponse
    :: Int -- ^ 'rpsrsResponseStatus'
    -> RejectPortfolioShareResponse
rejectPortfolioShareResponse pResponseStatus_ =
  RejectPortfolioShareResponse' {_rpsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rpsrsResponseStatus :: Lens' RejectPortfolioShareResponse Int
rpsrsResponseStatus = lens _rpsrsResponseStatus (\ s a -> s{_rpsrsResponseStatus = a})

instance NFData RejectPortfolioShareResponse where
