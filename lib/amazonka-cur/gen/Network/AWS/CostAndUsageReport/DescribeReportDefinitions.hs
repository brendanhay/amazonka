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
-- Module      : Network.AWS.CostAndUsageReport.DescribeReportDefinitions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a list of report definitions owned by the account
--
-- This operation returns paginated results.
module Network.AWS.CostAndUsageReport.DescribeReportDefinitions
    (
    -- * Creating a Request
      describeReportDefinitions
    , DescribeReportDefinitions
    -- * Request Lenses
    , drdNextToken
    , drdMaxResults

    -- * Destructuring the Response
    , describeReportDefinitionsResponse
    , DescribeReportDefinitionsResponse
    -- * Response Lenses
    , drdrsNextToken
    , drdrsReportDefinitions
    , drdrsResponseStatus
    ) where

import Network.AWS.CostAndUsageReport.Types
import Network.AWS.CostAndUsageReport.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of DescribeReportDefinitions
--
-- /See:/ 'describeReportDefinitions' smart constructor.
data DescribeReportDefinitions = DescribeReportDefinitions'
  { _drdNextToken  :: !(Maybe Text)
  , _drdMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReportDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdNextToken' - Undocumented member.
--
-- * 'drdMaxResults' - Undocumented member.
describeReportDefinitions
    :: DescribeReportDefinitions
describeReportDefinitions =
  DescribeReportDefinitions' {_drdNextToken = Nothing, _drdMaxResults = Nothing}


-- | Undocumented member.
drdNextToken :: Lens' DescribeReportDefinitions (Maybe Text)
drdNextToken = lens _drdNextToken (\ s a -> s{_drdNextToken = a})

-- | Undocumented member.
drdMaxResults :: Lens' DescribeReportDefinitions (Maybe Natural)
drdMaxResults = lens _drdMaxResults (\ s a -> s{_drdMaxResults = a}) . mapping _Nat

instance AWSPager DescribeReportDefinitions where
        page rq rs
          | stop (rs ^. drdrsNextToken) = Nothing
          | stop (rs ^. drdrsReportDefinitions) = Nothing
          | otherwise =
            Just $ rq & drdNextToken .~ rs ^. drdrsNextToken

instance AWSRequest DescribeReportDefinitions where
        type Rs DescribeReportDefinitions =
             DescribeReportDefinitionsResponse
        request = postJSON costAndUsageReport
        response
          = receiveJSON
              (\ s h x ->
                 DescribeReportDefinitionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ReportDefinitions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeReportDefinitions where

instance NFData DescribeReportDefinitions where

instance ToHeaders DescribeReportDefinitions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrigamiServiceGatewayService.DescribeReportDefinitions"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeReportDefinitions where
        toJSON DescribeReportDefinitions'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _drdNextToken,
                  ("MaxResults" .=) <$> _drdMaxResults])

instance ToPath DescribeReportDefinitions where
        toPath = const "/"

instance ToQuery DescribeReportDefinitions where
        toQuery = const mempty

-- | Response of DescribeReportDefinitions
--
-- /See:/ 'describeReportDefinitionsResponse' smart constructor.
data DescribeReportDefinitionsResponse = DescribeReportDefinitionsResponse'
  { _drdrsNextToken         :: !(Maybe Text)
  , _drdrsReportDefinitions :: !(Maybe [ReportDefinition])
  , _drdrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReportDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdrsNextToken' - Undocumented member.
--
-- * 'drdrsReportDefinitions' - Undocumented member.
--
-- * 'drdrsResponseStatus' - -- | The response status code.
describeReportDefinitionsResponse
    :: Int -- ^ 'drdrsResponseStatus'
    -> DescribeReportDefinitionsResponse
describeReportDefinitionsResponse pResponseStatus_ =
  DescribeReportDefinitionsResponse'
    { _drdrsNextToken = Nothing
    , _drdrsReportDefinitions = Nothing
    , _drdrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
drdrsNextToken :: Lens' DescribeReportDefinitionsResponse (Maybe Text)
drdrsNextToken = lens _drdrsNextToken (\ s a -> s{_drdrsNextToken = a})

-- | Undocumented member.
drdrsReportDefinitions :: Lens' DescribeReportDefinitionsResponse [ReportDefinition]
drdrsReportDefinitions = lens _drdrsReportDefinitions (\ s a -> s{_drdrsReportDefinitions = a}) . _Default . _Coerce

-- | -- | The response status code.
drdrsResponseStatus :: Lens' DescribeReportDefinitionsResponse Int
drdrsResponseStatus = lens _drdrsResponseStatus (\ s a -> s{_drdrsResponseStatus = a})

instance NFData DescribeReportDefinitionsResponse
         where
