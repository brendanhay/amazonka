{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateMLModel
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the @MLModelName@ and the @ScoreThreshold@ of an @MLModel@.
--
-- You can use the GetMLModel operation to view the contents of the updated
-- data element.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_UpdateMLModel.html>
module Network.AWS.MachineLearning.UpdateMLModel
    (
    -- * Request
      UpdateMLModel
    -- ** Request constructor
    , updateMLModel
    -- ** Request lenses
    , umlmrqMLModelName
    , umlmrqScoreThreshold
    , umlmrqMLModelId

    -- * Response
    , UpdateMLModelResponse
    -- ** Response constructor
    , updateMLModelResponse
    -- ** Response lenses
    , umlmrsMLModelId
    , umlmrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateMLModel' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umlmrqMLModelName'
--
-- * 'umlmrqScoreThreshold'
--
-- * 'umlmrqMLModelId'
data UpdateMLModel = UpdateMLModel'
    { _umlmrqMLModelName    :: !(Maybe Text)
    , _umlmrqScoreThreshold :: !(Maybe Double)
    , _umlmrqMLModelId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateMLModel' smart constructor.
updateMLModel :: Text -> UpdateMLModel
updateMLModel pMLModelId =
    UpdateMLModel'
    { _umlmrqMLModelName = Nothing
    , _umlmrqScoreThreshold = Nothing
    , _umlmrqMLModelId = pMLModelId
    }

-- | A user-supplied name or description of the @MLModel@.
umlmrqMLModelName :: Lens' UpdateMLModel (Maybe Text)
umlmrqMLModelName = lens _umlmrqMLModelName (\ s a -> s{_umlmrqMLModelName = a});

-- | The @ScoreThreshold@ used in binary classification @MLModel@ that marks
-- the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the @ScoreThreshold@ receive a
-- positive result from the @MLModel@, such as @true@. Output values less
-- than the @ScoreThreshold@ receive a negative response from the
-- @MLModel@, such as @false@.
umlmrqScoreThreshold :: Lens' UpdateMLModel (Maybe Double)
umlmrqScoreThreshold = lens _umlmrqScoreThreshold (\ s a -> s{_umlmrqScoreThreshold = a});

-- | The ID assigned to the @MLModel@ during creation.
umlmrqMLModelId :: Lens' UpdateMLModel Text
umlmrqMLModelId = lens _umlmrqMLModelId (\ s a -> s{_umlmrqMLModelId = a});

instance AWSRequest UpdateMLModel where
        type Sv UpdateMLModel = MachineLearning
        type Rs UpdateMLModel = UpdateMLModelResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateMLModelResponse' <$>
                   (x .?> "MLModelId") <*> (pure (fromEnum s)))

instance ToHeaders UpdateMLModel where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.UpdateMLModel" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateMLModel where
        toJSON UpdateMLModel'{..}
          = object
              ["MLModelName" .= _umlmrqMLModelName,
               "ScoreThreshold" .= _umlmrqScoreThreshold,
               "MLModelId" .= _umlmrqMLModelId]

instance ToPath UpdateMLModel where
        toPath = const "/"

instance ToQuery UpdateMLModel where
        toQuery = const mempty

-- | Represents the output of an UpdateMLModel operation.
--
-- You can see the updated content by using the GetMLModel operation.
--
-- /See:/ 'updateMLModelResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umlmrsMLModelId'
--
-- * 'umlmrsStatus'
data UpdateMLModelResponse = UpdateMLModelResponse'
    { _umlmrsMLModelId :: !(Maybe Text)
    , _umlmrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateMLModelResponse' smart constructor.
updateMLModelResponse :: Int -> UpdateMLModelResponse
updateMLModelResponse pStatus =
    UpdateMLModelResponse'
    { _umlmrsMLModelId = Nothing
    , _umlmrsStatus = pStatus
    }

-- | The ID assigned to the @MLModel@ during creation. This value should be
-- identical to the value of the @MLModelID@ in the request.
umlmrsMLModelId :: Lens' UpdateMLModelResponse (Maybe Text)
umlmrsMLModelId = lens _umlmrsMLModelId (\ s a -> s{_umlmrsMLModelId = a});

-- | FIXME: Undocumented member.
umlmrsStatus :: Lens' UpdateMLModelResponse Int
umlmrsStatus = lens _umlmrsStatus (\ s a -> s{_umlmrsStatus = a});
