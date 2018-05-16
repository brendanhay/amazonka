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
-- Module      : Network.AWS.Config.GetComplianceSummaryByResourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of resources that are compliant and the number that are noncompliant. You can specify one or more resource types to get these numbers for each resource type. The maximum number returned is 100.
--
--
module Network.AWS.Config.GetComplianceSummaryByResourceType
    (
    -- * Creating a Request
      getComplianceSummaryByResourceType
    , GetComplianceSummaryByResourceType
    -- * Request Lenses
    , gcsbrtResourceTypes

    -- * Destructuring the Response
    , getComplianceSummaryByResourceTypeResponse
    , GetComplianceSummaryByResourceTypeResponse
    -- * Response Lenses
    , gcsbrtrsComplianceSummariesByResourceType
    , gcsbrtrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'getComplianceSummaryByResourceType' smart constructor.
newtype GetComplianceSummaryByResourceType = GetComplianceSummaryByResourceType'
  { _gcsbrtResourceTypes :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetComplianceSummaryByResourceType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsbrtResourceTypes' - Specify one or more resource types to get the number of resources that are compliant and the number that are noncompliant for each resource type. For this request, you can specify an AWS resource type such as @AWS::EC2::Instance@ . You can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
getComplianceSummaryByResourceType
    :: GetComplianceSummaryByResourceType
getComplianceSummaryByResourceType =
  GetComplianceSummaryByResourceType' {_gcsbrtResourceTypes = Nothing}


-- | Specify one or more resource types to get the number of resources that are compliant and the number that are noncompliant for each resource type. For this request, you can specify an AWS resource type such as @AWS::EC2::Instance@ . You can specify that the resource type is an AWS account by specifying @AWS::::Account@ .
gcsbrtResourceTypes :: Lens' GetComplianceSummaryByResourceType [Text]
gcsbrtResourceTypes = lens _gcsbrtResourceTypes (\ s a -> s{_gcsbrtResourceTypes = a}) . _Default . _Coerce

instance AWSRequest
           GetComplianceSummaryByResourceType
         where
        type Rs GetComplianceSummaryByResourceType =
             GetComplianceSummaryByResourceTypeResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 GetComplianceSummaryByResourceTypeResponse' <$>
                   (x .?> "ComplianceSummariesByResourceType" .!@
                      mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetComplianceSummaryByResourceType
         where

instance NFData GetComplianceSummaryByResourceType
         where

instance ToHeaders GetComplianceSummaryByResourceType
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.GetComplianceSummaryByResourceType"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetComplianceSummaryByResourceType
         where
        toJSON GetComplianceSummaryByResourceType'{..}
          = object
              (catMaybes
                 [("ResourceTypes" .=) <$> _gcsbrtResourceTypes])

instance ToPath GetComplianceSummaryByResourceType
         where
        toPath = const "/"

instance ToQuery GetComplianceSummaryByResourceType
         where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'getComplianceSummaryByResourceTypeResponse' smart constructor.
data GetComplianceSummaryByResourceTypeResponse = GetComplianceSummaryByResourceTypeResponse'
  { _gcsbrtrsComplianceSummariesByResourceType :: !(Maybe [ComplianceSummaryByResourceType])
  , _gcsbrtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetComplianceSummaryByResourceTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsbrtrsComplianceSummariesByResourceType' - The number of resources that are compliant and the number that are noncompliant. If one or more resource types were provided with the request, the numbers are returned for each resource type. The maximum number returned is 100.
--
-- * 'gcsbrtrsResponseStatus' - -- | The response status code.
getComplianceSummaryByResourceTypeResponse
    :: Int -- ^ 'gcsbrtrsResponseStatus'
    -> GetComplianceSummaryByResourceTypeResponse
getComplianceSummaryByResourceTypeResponse pResponseStatus_ =
  GetComplianceSummaryByResourceTypeResponse'
    { _gcsbrtrsComplianceSummariesByResourceType = Nothing
    , _gcsbrtrsResponseStatus = pResponseStatus_
    }


-- | The number of resources that are compliant and the number that are noncompliant. If one or more resource types were provided with the request, the numbers are returned for each resource type. The maximum number returned is 100.
gcsbrtrsComplianceSummariesByResourceType :: Lens' GetComplianceSummaryByResourceTypeResponse [ComplianceSummaryByResourceType]
gcsbrtrsComplianceSummariesByResourceType = lens _gcsbrtrsComplianceSummariesByResourceType (\ s a -> s{_gcsbrtrsComplianceSummariesByResourceType = a}) . _Default . _Coerce

-- | -- | The response status code.
gcsbrtrsResponseStatus :: Lens' GetComplianceSummaryByResourceTypeResponse Int
gcsbrtrsResponseStatus = lens _gcsbrtrsResponseStatus (\ s a -> s{_gcsbrtrsResponseStatus = a})

instance NFData
           GetComplianceSummaryByResourceTypeResponse
         where
