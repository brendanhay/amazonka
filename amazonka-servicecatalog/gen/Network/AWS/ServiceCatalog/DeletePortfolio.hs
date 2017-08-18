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
-- Module      : Network.AWS.ServiceCatalog.DeletePortfolio
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified portfolio. This operation does not work with a portfolio that has been shared with you or if it has products, users, constraints, or shared accounts associated with it.
--
--
module Network.AWS.ServiceCatalog.DeletePortfolio
    (
    -- * Creating a Request
      deletePortfolio
    , DeletePortfolio
    -- * Request Lenses
    , dppAcceptLanguage
    , dppId

    -- * Destructuring the Response
    , deletePortfolioResponse
    , DeletePortfolioResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'deletePortfolio' smart constructor.
data DeletePortfolio = DeletePortfolio'
    { _dppAcceptLanguage :: !(Maybe Text)
    , _dppId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeletePortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dppAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'dppId' - The identifier of the portfolio for the delete request.
deletePortfolio
    :: Text -- ^ 'dppId'
    -> DeletePortfolio
deletePortfolio pId_ =
    DeletePortfolio'
    { _dppAcceptLanguage = Nothing
    , _dppId = pId_
    }

-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
dppAcceptLanguage :: Lens' DeletePortfolio (Maybe Text)
dppAcceptLanguage = lens _dppAcceptLanguage (\ s a -> s{_dppAcceptLanguage = a});

-- | The identifier of the portfolio for the delete request.
dppId :: Lens' DeletePortfolio Text
dppId = lens _dppId (\ s a -> s{_dppId = a});

instance AWSRequest DeletePortfolio where
        type Rs DeletePortfolio = DeletePortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DeletePortfolioResponse' <$> (pure (fromEnum s)))

instance Hashable DeletePortfolio

instance NFData DeletePortfolio

instance ToHeaders DeletePortfolio where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DeletePortfolio" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePortfolio where
        toJSON DeletePortfolio'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dppAcceptLanguage,
                  Just ("Id" .= _dppId)])

instance ToPath DeletePortfolio where
        toPath = const "/"

instance ToQuery DeletePortfolio where
        toQuery = const mempty

-- | /See:/ 'deletePortfolioResponse' smart constructor.
newtype DeletePortfolioResponse = DeletePortfolioResponse'
    { _delrsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeletePortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deletePortfolioResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeletePortfolioResponse
deletePortfolioResponse pResponseStatus_ =
    DeletePortfolioResponse'
    { _delrsResponseStatus = pResponseStatus_
    }

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeletePortfolioResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a});

instance NFData DeletePortfolioResponse
