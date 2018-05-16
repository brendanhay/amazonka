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
-- Module      : Network.AWS.Inspector.ListRulesPackages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available Amazon Inspector rules packages.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListRulesPackages
    (
    -- * Creating a Request
      listRulesPackages
    , ListRulesPackages
    -- * Request Lenses
    , lrpNextToken
    , lrpMaxResults

    -- * Destructuring the Response
    , listRulesPackagesResponse
    , ListRulesPackagesResponse
    -- * Response Lenses
    , lrprsNextToken
    , lrprsResponseStatus
    , lrprsRulesPackageARNs
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRulesPackages' smart constructor.
data ListRulesPackages = ListRulesPackages'
  { _lrpNextToken  :: !(Maybe Text)
  , _lrpMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRulesPackages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrpNextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListRulesPackages__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- * 'lrpMaxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
listRulesPackages
    :: ListRulesPackages
listRulesPackages =
  ListRulesPackages' {_lrpNextToken = Nothing, _lrpMaxResults = Nothing}


-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListRulesPackages__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
lrpNextToken :: Lens' ListRulesPackages (Maybe Text)
lrpNextToken = lens _lrpNextToken (\ s a -> s{_lrpNextToken = a})

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
lrpMaxResults :: Lens' ListRulesPackages (Maybe Int)
lrpMaxResults = lens _lrpMaxResults (\ s a -> s{_lrpMaxResults = a})

instance AWSPager ListRulesPackages where
        page rq rs
          | stop (rs ^. lrprsNextToken) = Nothing
          | stop (rs ^. lrprsRulesPackageARNs) = Nothing
          | otherwise =
            Just $ rq & lrpNextToken .~ rs ^. lrprsNextToken

instance AWSRequest ListRulesPackages where
        type Rs ListRulesPackages = ListRulesPackagesResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 ListRulesPackagesResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "rulesPackageArns" .!@ mempty))

instance Hashable ListRulesPackages where

instance NFData ListRulesPackages where

instance ToHeaders ListRulesPackages where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.ListRulesPackages" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRulesPackages where
        toJSON ListRulesPackages'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lrpNextToken,
                  ("maxResults" .=) <$> _lrpMaxResults])

instance ToPath ListRulesPackages where
        toPath = const "/"

instance ToQuery ListRulesPackages where
        toQuery = const mempty

-- | /See:/ 'listRulesPackagesResponse' smart constructor.
data ListRulesPackagesResponse = ListRulesPackagesResponse'
  { _lrprsNextToken        :: !(Maybe Text)
  , _lrprsResponseStatus   :: !Int
  , _lrprsRulesPackageARNs :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRulesPackagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrprsNextToken' - When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- * 'lrprsResponseStatus' - -- | The response status code.
--
-- * 'lrprsRulesPackageARNs' - The list of ARNs that specifies the rules packages returned by the action.
listRulesPackagesResponse
    :: Int -- ^ 'lrprsResponseStatus'
    -> ListRulesPackagesResponse
listRulesPackagesResponse pResponseStatus_ =
  ListRulesPackagesResponse'
    { _lrprsNextToken = Nothing
    , _lrprsResponseStatus = pResponseStatus_
    , _lrprsRulesPackageARNs = mempty
    }


-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
lrprsNextToken :: Lens' ListRulesPackagesResponse (Maybe Text)
lrprsNextToken = lens _lrprsNextToken (\ s a -> s{_lrprsNextToken = a})

-- | -- | The response status code.
lrprsResponseStatus :: Lens' ListRulesPackagesResponse Int
lrprsResponseStatus = lens _lrprsResponseStatus (\ s a -> s{_lrprsResponseStatus = a})

-- | The list of ARNs that specifies the rules packages returned by the action.
lrprsRulesPackageARNs :: Lens' ListRulesPackagesResponse [Text]
lrprsRulesPackageARNs = lens _lrprsRulesPackageARNs (\ s a -> s{_lrprsRulesPackageARNs = a}) . _Coerce

instance NFData ListRulesPackagesResponse where
