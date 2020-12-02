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
-- Module      : Network.AWS.Inspector.DescribeFindings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the findings that are specified by the ARNs of the findings.
--
--
module Network.AWS.Inspector.DescribeFindings
    (
    -- * Creating a Request
      describeFindings
    , DescribeFindings
    -- * Request Lenses
    , dfLocale
    , dfFindingARNs

    -- * Destructuring the Response
    , describeFindingsResponse
    , DescribeFindingsResponse
    -- * Response Lenses
    , dfrsResponseStatus
    , dfrsFindings
    , dfrsFailedItems
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFindings' smart constructor.
data DescribeFindings = DescribeFindings'
  { _dfLocale      :: !(Maybe Locale)
  , _dfFindingARNs :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfLocale' - The locale into which you want to translate a finding description, recommendation, and the short description that identifies the finding.
--
-- * 'dfFindingARNs' - The ARN that specifies the finding that you want to describe.
describeFindings
    :: NonEmpty Text -- ^ 'dfFindingARNs'
    -> DescribeFindings
describeFindings pFindingARNs_ =
  DescribeFindings'
    {_dfLocale = Nothing, _dfFindingARNs = _List1 # pFindingARNs_}


-- | The locale into which you want to translate a finding description, recommendation, and the short description that identifies the finding.
dfLocale :: Lens' DescribeFindings (Maybe Locale)
dfLocale = lens _dfLocale (\ s a -> s{_dfLocale = a})

-- | The ARN that specifies the finding that you want to describe.
dfFindingARNs :: Lens' DescribeFindings (NonEmpty Text)
dfFindingARNs = lens _dfFindingARNs (\ s a -> s{_dfFindingARNs = a}) . _List1

instance AWSRequest DescribeFindings where
        type Rs DescribeFindings = DescribeFindingsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFindingsResponse' <$>
                   (pure (fromEnum s)) <*> (x .?> "findings" .!@ mempty)
                     <*> (x .?> "failedItems" .!@ mempty))

instance Hashable DescribeFindings where

instance NFData DescribeFindings where

instance ToHeaders DescribeFindings where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeFindings" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeFindings where
        toJSON DescribeFindings'{..}
          = object
              (catMaybes
                 [("locale" .=) <$> _dfLocale,
                  Just ("findingArns" .= _dfFindingARNs)])

instance ToPath DescribeFindings where
        toPath = const "/"

instance ToQuery DescribeFindings where
        toQuery = const mempty

-- | /See:/ 'describeFindingsResponse' smart constructor.
data DescribeFindingsResponse = DescribeFindingsResponse'
  { _dfrsResponseStatus :: !Int
  , _dfrsFindings       :: ![Finding]
  , _dfrsFailedItems    :: !(Map Text FailedItemDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfrsResponseStatus' - -- | The response status code.
--
-- * 'dfrsFindings' - Information about the finding.
--
-- * 'dfrsFailedItems' - Finding details that cannot be described. An error code is provided for each failed item.
describeFindingsResponse
    :: Int -- ^ 'dfrsResponseStatus'
    -> DescribeFindingsResponse
describeFindingsResponse pResponseStatus_ =
  DescribeFindingsResponse'
    { _dfrsResponseStatus = pResponseStatus_
    , _dfrsFindings = mempty
    , _dfrsFailedItems = mempty
    }


-- | -- | The response status code.
dfrsResponseStatus :: Lens' DescribeFindingsResponse Int
dfrsResponseStatus = lens _dfrsResponseStatus (\ s a -> s{_dfrsResponseStatus = a})

-- | Information about the finding.
dfrsFindings :: Lens' DescribeFindingsResponse [Finding]
dfrsFindings = lens _dfrsFindings (\ s a -> s{_dfrsFindings = a}) . _Coerce

-- | Finding details that cannot be described. An error code is provided for each failed item.
dfrsFailedItems :: Lens' DescribeFindingsResponse (HashMap Text FailedItemDetails)
dfrsFailedItems = lens _dfrsFailedItems (\ s a -> s{_dfrsFailedItems = a}) . _Map

instance NFData DescribeFindingsResponse where
