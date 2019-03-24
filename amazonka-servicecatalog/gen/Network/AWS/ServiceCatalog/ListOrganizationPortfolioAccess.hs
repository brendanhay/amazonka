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
-- Module      : Network.AWS.ServiceCatalog.ListOrganizationPortfolioAccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the organization nodes that have access to the specified portfolio. This API can only be called by the master account in the organization.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListOrganizationPortfolioAccess
    (
    -- * Creating a Request
      listOrganizationPortfolioAccess
    , ListOrganizationPortfolioAccess
    -- * Request Lenses
    , lopaAcceptLanguage
    , lopaPageToken
    , lopaPageSize
    , lopaPortfolioId
    , lopaOrganizationNodeType

    -- * Destructuring the Response
    , listOrganizationPortfolioAccessResponse
    , ListOrganizationPortfolioAccessResponse
    -- * Response Lenses
    , loparsNextPageToken
    , loparsOrganizationNodes
    , loparsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listOrganizationPortfolioAccess' smart constructor.
data ListOrganizationPortfolioAccess = ListOrganizationPortfolioAccess'
  { _lopaAcceptLanguage       :: !(Maybe Text)
  , _lopaPageToken            :: !(Maybe Text)
  , _lopaPageSize             :: !(Maybe Nat)
  , _lopaPortfolioId          :: !Text
  , _lopaOrganizationNodeType :: !OrganizationNodeType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOrganizationPortfolioAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lopaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lopaPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'lopaPageSize' - The maximum number of items to return with this call.
--
-- * 'lopaPortfolioId' - The portfolio identifier. For example, @port-2abcdext3y5fk@ .
--
-- * 'lopaOrganizationNodeType' - The organization node type that will be returned in the output.     * @ORGANIZATION@ - Organization that has access to the portfolio.      * @ORGANIZATIONAL_UNIT@ - Organizational unit that has access to the portfolio within your organization.     * @ACCOUNT@ - Account that has access to the portfolio within your organization.
listOrganizationPortfolioAccess
    :: Text -- ^ 'lopaPortfolioId'
    -> OrganizationNodeType -- ^ 'lopaOrganizationNodeType'
    -> ListOrganizationPortfolioAccess
listOrganizationPortfolioAccess pPortfolioId_ pOrganizationNodeType_ =
  ListOrganizationPortfolioAccess'
    { _lopaAcceptLanguage = Nothing
    , _lopaPageToken = Nothing
    , _lopaPageSize = Nothing
    , _lopaPortfolioId = pPortfolioId_
    , _lopaOrganizationNodeType = pOrganizationNodeType_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lopaAcceptLanguage :: Lens' ListOrganizationPortfolioAccess (Maybe Text)
lopaAcceptLanguage = lens _lopaAcceptLanguage (\ s a -> s{_lopaAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
lopaPageToken :: Lens' ListOrganizationPortfolioAccess (Maybe Text)
lopaPageToken = lens _lopaPageToken (\ s a -> s{_lopaPageToken = a})

-- | The maximum number of items to return with this call.
lopaPageSize :: Lens' ListOrganizationPortfolioAccess (Maybe Natural)
lopaPageSize = lens _lopaPageSize (\ s a -> s{_lopaPageSize = a}) . mapping _Nat

-- | The portfolio identifier. For example, @port-2abcdext3y5fk@ .
lopaPortfolioId :: Lens' ListOrganizationPortfolioAccess Text
lopaPortfolioId = lens _lopaPortfolioId (\ s a -> s{_lopaPortfolioId = a})

-- | The organization node type that will be returned in the output.     * @ORGANIZATION@ - Organization that has access to the portfolio.      * @ORGANIZATIONAL_UNIT@ - Organizational unit that has access to the portfolio within your organization.     * @ACCOUNT@ - Account that has access to the portfolio within your organization.
lopaOrganizationNodeType :: Lens' ListOrganizationPortfolioAccess OrganizationNodeType
lopaOrganizationNodeType = lens _lopaOrganizationNodeType (\ s a -> s{_lopaOrganizationNodeType = a})

instance AWSPager ListOrganizationPortfolioAccess
         where
        page rq rs
          | stop (rs ^. loparsNextPageToken) = Nothing
          | stop (rs ^. loparsOrganizationNodes) = Nothing
          | otherwise =
            Just $ rq &
              lopaPageToken .~ rs ^. loparsNextPageToken

instance AWSRequest ListOrganizationPortfolioAccess
         where
        type Rs ListOrganizationPortfolioAccess =
             ListOrganizationPortfolioAccessResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListOrganizationPortfolioAccessResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "OrganizationNodes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListOrganizationPortfolioAccess
         where

instance NFData ListOrganizationPortfolioAccess where

instance ToHeaders ListOrganizationPortfolioAccess
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListOrganizationPortfolioAccess"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListOrganizationPortfolioAccess where
        toJSON ListOrganizationPortfolioAccess'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _lopaAcceptLanguage,
                  ("PageToken" .=) <$> _lopaPageToken,
                  ("PageSize" .=) <$> _lopaPageSize,
                  Just ("PortfolioId" .= _lopaPortfolioId),
                  Just
                    ("OrganizationNodeType" .=
                       _lopaOrganizationNodeType)])

instance ToPath ListOrganizationPortfolioAccess where
        toPath = const "/"

instance ToQuery ListOrganizationPortfolioAccess
         where
        toQuery = const mempty

-- | /See:/ 'listOrganizationPortfolioAccessResponse' smart constructor.
data ListOrganizationPortfolioAccessResponse = ListOrganizationPortfolioAccessResponse'
  { _loparsNextPageToken     :: !(Maybe Text)
  , _loparsOrganizationNodes :: !(Maybe [OrganizationNode])
  , _loparsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOrganizationPortfolioAccessResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loparsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'loparsOrganizationNodes' - Displays information about the organization nodes.
--
-- * 'loparsResponseStatus' - -- | The response status code.
listOrganizationPortfolioAccessResponse
    :: Int -- ^ 'loparsResponseStatus'
    -> ListOrganizationPortfolioAccessResponse
listOrganizationPortfolioAccessResponse pResponseStatus_ =
  ListOrganizationPortfolioAccessResponse'
    { _loparsNextPageToken = Nothing
    , _loparsOrganizationNodes = Nothing
    , _loparsResponseStatus = pResponseStatus_
    }


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
loparsNextPageToken :: Lens' ListOrganizationPortfolioAccessResponse (Maybe Text)
loparsNextPageToken = lens _loparsNextPageToken (\ s a -> s{_loparsNextPageToken = a})

-- | Displays information about the organization nodes.
loparsOrganizationNodes :: Lens' ListOrganizationPortfolioAccessResponse [OrganizationNode]
loparsOrganizationNodes = lens _loparsOrganizationNodes (\ s a -> s{_loparsOrganizationNodes = a}) . _Default . _Coerce

-- | -- | The response status code.
loparsResponseStatus :: Lens' ListOrganizationPortfolioAccessResponse Int
loparsResponseStatus = lens _loparsResponseStatus (\ s a -> s{_loparsResponseStatus = a})

instance NFData
           ListOrganizationPortfolioAccessResponse
         where
