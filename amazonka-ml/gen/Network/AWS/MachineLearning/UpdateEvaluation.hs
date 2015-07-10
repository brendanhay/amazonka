{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateEvaluation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the @EvaluationName@ of an @Evaluation@.
--
-- You can use the GetEvaluation operation to view the contents of the
-- updated data element.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_UpdateEvaluation.html>
module Network.AWS.MachineLearning.UpdateEvaluation
    (
    -- * Request
      UpdateEvaluation
    -- ** Request constructor
    , updateEvaluation
    -- ** Request lenses
    , ueEvaluationId
    , ueEvaluationName

    -- * Response
    , UpdateEvaluationResponse
    -- ** Response constructor
    , updateEvaluationResponse
    -- ** Response lenses
    , uerEvaluationId
    , uerStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateEvaluation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ueEvaluationId'
--
-- * 'ueEvaluationName'
data UpdateEvaluation = UpdateEvaluation'
    { _ueEvaluationId   :: !Text
    , _ueEvaluationName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateEvaluation' smart constructor.
updateEvaluation :: Text -> Text -> UpdateEvaluation
updateEvaluation pEvaluationId pEvaluationName =
    UpdateEvaluation'
    { _ueEvaluationId = pEvaluationId
    , _ueEvaluationName = pEvaluationName
    }

-- | The ID assigned to the @Evaluation@ during creation.
ueEvaluationId :: Lens' UpdateEvaluation Text
ueEvaluationId = lens _ueEvaluationId (\ s a -> s{_ueEvaluationId = a});

-- | A new user-supplied name or description of the @Evaluation@ that will
-- replace the current content.
ueEvaluationName :: Lens' UpdateEvaluation Text
ueEvaluationName = lens _ueEvaluationName (\ s a -> s{_ueEvaluationName = a});

instance AWSRequest UpdateEvaluation where
        type Sv UpdateEvaluation = MachineLearning
        type Rs UpdateEvaluation = UpdateEvaluationResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateEvaluationResponse' <$>
                   (x .?> "EvaluationId") <*> (pure (fromEnum s)))

instance ToHeaders UpdateEvaluation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.UpdateEvaluation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateEvaluation where
        toJSON UpdateEvaluation'{..}
          = object
              ["EvaluationId" .= _ueEvaluationId,
               "EvaluationName" .= _ueEvaluationName]

instance ToPath UpdateEvaluation where
        toPath = const "/"

instance ToQuery UpdateEvaluation where
        toQuery = const mempty

-- | Represents the output of an UpdateEvaluation operation.
--
-- You can see the updated content by using the GetEvaluation operation.
--
-- /See:/ 'updateEvaluationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uerEvaluationId'
--
-- * 'uerStatus'
data UpdateEvaluationResponse = UpdateEvaluationResponse'
    { _uerEvaluationId :: !(Maybe Text)
    , _uerStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateEvaluationResponse' smart constructor.
updateEvaluationResponse :: Int -> UpdateEvaluationResponse
updateEvaluationResponse pStatus =
    UpdateEvaluationResponse'
    { _uerEvaluationId = Nothing
    , _uerStatus = pStatus
    }

-- | The ID assigned to the @Evaluation@ during creation. This value should
-- be identical to the value of the @Evaluation@ in the request.
uerEvaluationId :: Lens' UpdateEvaluationResponse (Maybe Text)
uerEvaluationId = lens _uerEvaluationId (\ s a -> s{_uerEvaluationId = a});

-- | FIXME: Undocumented member.
uerStatus :: Lens' UpdateEvaluationResponse Int
uerStatus = lens _uerStatus (\ s a -> s{_uerStatus = a});
