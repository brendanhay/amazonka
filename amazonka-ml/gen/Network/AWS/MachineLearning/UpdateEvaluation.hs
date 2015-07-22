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
    , uerqEvaluationId
    , uerqEvaluationName

    -- * Response
    , UpdateEvaluationResponse
    -- ** Response constructor
    , updateEvaluationResponse
    -- ** Response lenses
    , uersEvaluationId
    , uersStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateEvaluation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uerqEvaluationId'
--
-- * 'uerqEvaluationName'
data UpdateEvaluation = UpdateEvaluation'
    { _uerqEvaluationId   :: !Text
    , _uerqEvaluationName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateEvaluation' smart constructor.
updateEvaluation :: Text -> Text -> UpdateEvaluation
updateEvaluation pEvaluationId pEvaluationName =
    UpdateEvaluation'
    { _uerqEvaluationId = pEvaluationId
    , _uerqEvaluationName = pEvaluationName
    }

-- | The ID assigned to the @Evaluation@ during creation.
uerqEvaluationId :: Lens' UpdateEvaluation Text
uerqEvaluationId = lens _uerqEvaluationId (\ s a -> s{_uerqEvaluationId = a});

-- | A new user-supplied name or description of the @Evaluation@ that will
-- replace the current content.
uerqEvaluationName :: Lens' UpdateEvaluation Text
uerqEvaluationName = lens _uerqEvaluationName (\ s a -> s{_uerqEvaluationName = a});

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
              ["EvaluationId" .= _uerqEvaluationId,
               "EvaluationName" .= _uerqEvaluationName]

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
-- * 'uersEvaluationId'
--
-- * 'uersStatus'
data UpdateEvaluationResponse = UpdateEvaluationResponse'
    { _uersEvaluationId :: !(Maybe Text)
    , _uersStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateEvaluationResponse' smart constructor.
updateEvaluationResponse :: Int -> UpdateEvaluationResponse
updateEvaluationResponse pStatus =
    UpdateEvaluationResponse'
    { _uersEvaluationId = Nothing
    , _uersStatus = pStatus
    }

-- | The ID assigned to the @Evaluation@ during creation. This value should
-- be identical to the value of the @Evaluation@ in the request.
uersEvaluationId :: Lens' UpdateEvaluationResponse (Maybe Text)
uersEvaluationId = lens _uersEvaluationId (\ s a -> s{_uersEvaluationId = a});

-- | FIXME: Undocumented member.
uersStatus :: Lens' UpdateEvaluationResponse Int
uersStatus = lens _uersStatus (\ s a -> s{_uersStatus = a});
