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
-- Module      : Network.AWS.Config.GetComplianceDetailsByResource
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified AWS resource. The results indicate which AWS Config rules were used to evaluate the resource, when each rule was last used, and whether the resource complies with each rule.
module Network.AWS.Config.GetComplianceDetailsByResource
    (
    -- * Creating a Request
      getComplianceDetailsByResource
    , GetComplianceDetailsByResource
    -- * Request Lenses
    , gcdbrComplianceTypes
    , gcdbrNextToken
    , gcdbrResourceType
    , gcdbrResourceId

    -- * Destructuring the Response
    , getComplianceDetailsByResourceResponse
    , GetComplianceDetailsByResourceResponse
    -- * Response Lenses
    , gcdbrrsEvaluationResults
    , gcdbrrsNextToken
    , gcdbrrsResponseStatus
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Config.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getComplianceDetailsByResource' smart constructor.
data GetComplianceDetailsByResource = GetComplianceDetailsByResource'
    { _gcdbrComplianceTypes :: !(Maybe [ComplianceType])
    , _gcdbrNextToken       :: !(Maybe Text)
    , _gcdbrResourceType    :: !Text
    , _gcdbrResourceId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetComplianceDetailsByResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdbrComplianceTypes'
--
-- * 'gcdbrNextToken'
--
-- * 'gcdbrResourceType'
--
-- * 'gcdbrResourceId'
getComplianceDetailsByResource
    :: Text -- ^ 'gcdbrResourceType'
    -> Text -- ^ 'gcdbrResourceId'
    -> GetComplianceDetailsByResource
getComplianceDetailsByResource pResourceType_ pResourceId_ =
    GetComplianceDetailsByResource'
    { _gcdbrComplianceTypes = Nothing
    , _gcdbrNextToken = Nothing
    , _gcdbrResourceType = pResourceType_
    , _gcdbrResourceId = pResourceId_
    }

-- | Filters the results by compliance.
--
-- The allowed values are 'COMPLIANT', 'NON_COMPLIANT', and 'NOT_APPLICABLE'.
gcdbrComplianceTypes :: Lens' GetComplianceDetailsByResource [ComplianceType]
gcdbrComplianceTypes = lens _gcdbrComplianceTypes (\ s a -> s{_gcdbrComplianceTypes = a}) . _Default . _Coerce;

-- | The 'nextToken' string returned on a previous page that you use to get the next page of results in a paginated response.
gcdbrNextToken :: Lens' GetComplianceDetailsByResource (Maybe Text)
gcdbrNextToken = lens _gcdbrNextToken (\ s a -> s{_gcdbrNextToken = a});

-- | The type of the AWS resource for which you want compliance information.
gcdbrResourceType :: Lens' GetComplianceDetailsByResource Text
gcdbrResourceType = lens _gcdbrResourceType (\ s a -> s{_gcdbrResourceType = a});

-- | The ID of the AWS resource for which you want compliance information.
gcdbrResourceId :: Lens' GetComplianceDetailsByResource Text
gcdbrResourceId = lens _gcdbrResourceId (\ s a -> s{_gcdbrResourceId = a});

instance AWSRequest GetComplianceDetailsByResource
         where
        type Rs GetComplianceDetailsByResource =
             GetComplianceDetailsByResourceResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 GetComplianceDetailsByResourceResponse' <$>
                   (x .?> "EvaluationResults" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetComplianceDetailsByResource

instance NFData GetComplianceDetailsByResource

instance ToHeaders GetComplianceDetailsByResource
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.GetComplianceDetailsByResource"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetComplianceDetailsByResource where
        toJSON GetComplianceDetailsByResource'{..}
          = object
              (catMaybes
                 [("ComplianceTypes" .=) <$> _gcdbrComplianceTypes,
                  ("NextToken" .=) <$> _gcdbrNextToken,
                  Just ("ResourceType" .= _gcdbrResourceType),
                  Just ("ResourceId" .= _gcdbrResourceId)])

instance ToPath GetComplianceDetailsByResource where
        toPath = const "/"

instance ToQuery GetComplianceDetailsByResource where
        toQuery = const mempty

-- | /See:/ 'getComplianceDetailsByResourceResponse' smart constructor.
data GetComplianceDetailsByResourceResponse = GetComplianceDetailsByResourceResponse'
    { _gcdbrrsEvaluationResults :: !(Maybe [EvaluationResult])
    , _gcdbrrsNextToken         :: !(Maybe Text)
    , _gcdbrrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetComplianceDetailsByResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdbrrsEvaluationResults'
--
-- * 'gcdbrrsNextToken'
--
-- * 'gcdbrrsResponseStatus'
getComplianceDetailsByResourceResponse
    :: Int -- ^ 'gcdbrrsResponseStatus'
    -> GetComplianceDetailsByResourceResponse
getComplianceDetailsByResourceResponse pResponseStatus_ =
    GetComplianceDetailsByResourceResponse'
    { _gcdbrrsEvaluationResults = Nothing
    , _gcdbrrsNextToken = Nothing
    , _gcdbrrsResponseStatus = pResponseStatus_
    }

-- | Indicates whether the specified AWS resource complies each AWS Config rule.
gcdbrrsEvaluationResults :: Lens' GetComplianceDetailsByResourceResponse [EvaluationResult]
gcdbrrsEvaluationResults = lens _gcdbrrsEvaluationResults (\ s a -> s{_gcdbrrsEvaluationResults = a}) . _Default . _Coerce;

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
gcdbrrsNextToken :: Lens' GetComplianceDetailsByResourceResponse (Maybe Text)
gcdbrrsNextToken = lens _gcdbrrsNextToken (\ s a -> s{_gcdbrrsNextToken = a});

-- | The response status code.
gcdbrrsResponseStatus :: Lens' GetComplianceDetailsByResourceResponse Int
gcdbrrsResponseStatus = lens _gcdbrrsResponseStatus (\ s a -> s{_gcdbrrsResponseStatus = a});

instance NFData
         GetComplianceDetailsByResourceResponse
