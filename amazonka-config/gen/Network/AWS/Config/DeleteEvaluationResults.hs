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
-- Module      : Network.AWS.Config.DeleteEvaluationResults
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the evaluation results for the specified AWS Config rule. You can specify one AWS Config rule per request. After you delete the evaluation results, you can call the 'StartConfigRulesEvaluation' API to start evaluating your AWS resources against the rule.
--
--
module Network.AWS.Config.DeleteEvaluationResults
    (
    -- * Creating a Request
      deleteEvaluationResults
    , DeleteEvaluationResults
    -- * Request Lenses
    , derConfigRuleName

    -- * Destructuring the Response
    , deleteEvaluationResultsResponse
    , DeleteEvaluationResultsResponse
    -- * Response Lenses
    , derrsResponseStatus
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
-- /See:/ 'deleteEvaluationResults' smart constructor.
newtype DeleteEvaluationResults = DeleteEvaluationResults'
  { _derConfigRuleName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEvaluationResults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derConfigRuleName' - The name of the AWS Config rule for which you want to delete the evaluation results.
deleteEvaluationResults
    :: Text -- ^ 'derConfigRuleName'
    -> DeleteEvaluationResults
deleteEvaluationResults pConfigRuleName_ =
  DeleteEvaluationResults' {_derConfigRuleName = pConfigRuleName_}


-- | The name of the AWS Config rule for which you want to delete the evaluation results.
derConfigRuleName :: Lens' DeleteEvaluationResults Text
derConfigRuleName = lens _derConfigRuleName (\ s a -> s{_derConfigRuleName = a})

instance AWSRequest DeleteEvaluationResults where
        type Rs DeleteEvaluationResults =
             DeleteEvaluationResultsResponse
        request = postJSON config
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteEvaluationResultsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteEvaluationResults where

instance NFData DeleteEvaluationResults where

instance ToHeaders DeleteEvaluationResults where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DeleteEvaluationResults" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteEvaluationResults where
        toJSON DeleteEvaluationResults'{..}
          = object
              (catMaybes
                 [Just ("ConfigRuleName" .= _derConfigRuleName)])

instance ToPath DeleteEvaluationResults where
        toPath = const "/"

instance ToQuery DeleteEvaluationResults where
        toQuery = const mempty

-- | The output when you delete the evaluation results for the specified AWS Config rule.
--
--
--
-- /See:/ 'deleteEvaluationResultsResponse' smart constructor.
newtype DeleteEvaluationResultsResponse = DeleteEvaluationResultsResponse'
  { _derrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEvaluationResultsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derrsResponseStatus' - -- | The response status code.
deleteEvaluationResultsResponse
    :: Int -- ^ 'derrsResponseStatus'
    -> DeleteEvaluationResultsResponse
deleteEvaluationResultsResponse pResponseStatus_ =
  DeleteEvaluationResultsResponse' {_derrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
derrsResponseStatus :: Lens' DeleteEvaluationResultsResponse Int
derrsResponseStatus = lens _derrsResponseStatus (\ s a -> s{_derrsResponseStatus = a})

instance NFData DeleteEvaluationResultsResponse where
