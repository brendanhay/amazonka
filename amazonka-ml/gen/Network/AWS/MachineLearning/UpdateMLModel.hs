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
-- Module      : Network.AWS.MachineLearning.UpdateMLModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @MLModelName@ and the @ScoreThreshold@ of an @MLModel@ .
--
--
-- You can use the @GetMLModel@ operation to view the contents of the updated data element.
--
module Network.AWS.MachineLearning.UpdateMLModel
    (
    -- * Creating a Request
      updateMLModel
    , UpdateMLModel
    -- * Request Lenses
    , umlmMLModelName
    , umlmScoreThreshold
    , umlmMLModelId

    -- * Destructuring the Response
    , updateMLModelResponse
    , UpdateMLModelResponse
    -- * Response Lenses
    , umlmrsMLModelId
    , umlmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateMLModel' smart constructor.
data UpdateMLModel = UpdateMLModel'
  { _umlmMLModelName    :: !(Maybe Text)
  , _umlmScoreThreshold :: !(Maybe Double)
  , _umlmMLModelId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateMLModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umlmMLModelName' - A user-supplied name or description of the @MLModel@ .
--
-- * 'umlmScoreThreshold' - The @ScoreThreshold@ used in binary classification @MLModel@ that marks the boundary between a positive prediction and a negative prediction. Output values greater than or equal to the @ScoreThreshold@ receive a positive result from the @MLModel@ , such as @true@ . Output values less than the @ScoreThreshold@ receive a negative response from the @MLModel@ , such as @false@ .
--
-- * 'umlmMLModelId' - The ID assigned to the @MLModel@ during creation.
updateMLModel
    :: Text -- ^ 'umlmMLModelId'
    -> UpdateMLModel
updateMLModel pMLModelId_ =
  UpdateMLModel'
    { _umlmMLModelName = Nothing
    , _umlmScoreThreshold = Nothing
    , _umlmMLModelId = pMLModelId_
    }


-- | A user-supplied name or description of the @MLModel@ .
umlmMLModelName :: Lens' UpdateMLModel (Maybe Text)
umlmMLModelName = lens _umlmMLModelName (\ s a -> s{_umlmMLModelName = a})

-- | The @ScoreThreshold@ used in binary classification @MLModel@ that marks the boundary between a positive prediction and a negative prediction. Output values greater than or equal to the @ScoreThreshold@ receive a positive result from the @MLModel@ , such as @true@ . Output values less than the @ScoreThreshold@ receive a negative response from the @MLModel@ , such as @false@ .
umlmScoreThreshold :: Lens' UpdateMLModel (Maybe Double)
umlmScoreThreshold = lens _umlmScoreThreshold (\ s a -> s{_umlmScoreThreshold = a})

-- | The ID assigned to the @MLModel@ during creation.
umlmMLModelId :: Lens' UpdateMLModel Text
umlmMLModelId = lens _umlmMLModelId (\ s a -> s{_umlmMLModelId = a})

instance AWSRequest UpdateMLModel where
        type Rs UpdateMLModel = UpdateMLModelResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 UpdateMLModelResponse' <$>
                   (x .?> "MLModelId") <*> (pure (fromEnum s)))

instance Hashable UpdateMLModel where

instance NFData UpdateMLModel where

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
              (catMaybes
                 [("MLModelName" .=) <$> _umlmMLModelName,
                  ("ScoreThreshold" .=) <$> _umlmScoreThreshold,
                  Just ("MLModelId" .= _umlmMLModelId)])

instance ToPath UpdateMLModel where
        toPath = const "/"

instance ToQuery UpdateMLModel where
        toQuery = const mempty

-- | Represents the output of an @UpdateMLModel@ operation.
--
--
-- You can see the updated content by using the @GetMLModel@ operation.
--
--
-- /See:/ 'updateMLModelResponse' smart constructor.
data UpdateMLModelResponse = UpdateMLModelResponse'
  { _umlmrsMLModelId      :: !(Maybe Text)
  , _umlmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateMLModelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umlmrsMLModelId' - The ID assigned to the @MLModel@ during creation. This value should be identical to the value of the @MLModelID@ in the request.
--
-- * 'umlmrsResponseStatus' - -- | The response status code.
updateMLModelResponse
    :: Int -- ^ 'umlmrsResponseStatus'
    -> UpdateMLModelResponse
updateMLModelResponse pResponseStatus_ =
  UpdateMLModelResponse'
    {_umlmrsMLModelId = Nothing, _umlmrsResponseStatus = pResponseStatus_}


-- | The ID assigned to the @MLModel@ during creation. This value should be identical to the value of the @MLModelID@ in the request.
umlmrsMLModelId :: Lens' UpdateMLModelResponse (Maybe Text)
umlmrsMLModelId = lens _umlmrsMLModelId (\ s a -> s{_umlmrsMLModelId = a})

-- | -- | The response status code.
umlmrsResponseStatus :: Lens' UpdateMLModelResponse Int
umlmrsResponseStatus = lens _umlmrsResponseStatus (\ s a -> s{_umlmrsResponseStatus = a})

instance NFData UpdateMLModelResponse where
