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
    , umlmMLModelName
    , umlmScoreThreshold
    , umlmMLModelId

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
-- * 'umlmMLModelName'
--
-- * 'umlmScoreThreshold'
--
-- * 'umlmMLModelId'
data UpdateMLModel = UpdateMLModel'
    { _umlmMLModelName    :: !(Maybe Text)
    , _umlmScoreThreshold :: !(Maybe Double)
    , _umlmMLModelId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateMLModel' smart constructor.
updateMLModel :: Text -> UpdateMLModel
updateMLModel pMLModelId_ =
    UpdateMLModel'
    { _umlmMLModelName = Nothing
    , _umlmScoreThreshold = Nothing
    , _umlmMLModelId = pMLModelId_
    }

-- | A user-supplied name or description of the @MLModel@.
umlmMLModelName :: Lens' UpdateMLModel (Maybe Text)
umlmMLModelName = lens _umlmMLModelName (\ s a -> s{_umlmMLModelName = a});

-- | The @ScoreThreshold@ used in binary classification @MLModel@ that marks
-- the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the @ScoreThreshold@ receive a
-- positive result from the @MLModel@, such as @true@. Output values less
-- than the @ScoreThreshold@ receive a negative response from the
-- @MLModel@, such as @false@.
umlmScoreThreshold :: Lens' UpdateMLModel (Maybe Double)
umlmScoreThreshold = lens _umlmScoreThreshold (\ s a -> s{_umlmScoreThreshold = a});

-- | The ID assigned to the @MLModel@ during creation.
umlmMLModelId :: Lens' UpdateMLModel Text
umlmMLModelId = lens _umlmMLModelId (\ s a -> s{_umlmMLModelId = a});

instance AWSRequest UpdateMLModel where
        type Sv UpdateMLModel = MachineLearning
        type Rs UpdateMLModel = UpdateMLModelResponse
        request = postJSON "UpdateMLModel"
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
              ["MLModelName" .= _umlmMLModelName,
               "ScoreThreshold" .= _umlmScoreThreshold,
               "MLModelId" .= _umlmMLModelId]

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
updateMLModelResponse pStatus_ =
    UpdateMLModelResponse'
    { _umlmrsMLModelId = Nothing
    , _umlmrsStatus = pStatus_
    }

-- | The ID assigned to the @MLModel@ during creation. This value should be
-- identical to the value of the @MLModelID@ in the request.
umlmrsMLModelId :: Lens' UpdateMLModelResponse (Maybe Text)
umlmrsMLModelId = lens _umlmrsMLModelId (\ s a -> s{_umlmrsMLModelId = a});

-- | FIXME: Undocumented member.
umlmrsStatus :: Lens' UpdateMLModelResponse Int
umlmrsStatus = lens _umlmrsStatus (\ s a -> s{_umlmrsStatus = a});
