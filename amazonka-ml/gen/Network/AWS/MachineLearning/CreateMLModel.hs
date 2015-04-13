{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.MachineLearning.CreateMLModel
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new 'MLModel' using the data files and the recipe as information
-- sources.
--
-- An 'MLModel' is nearly immutable. Users can only update the 'MLModelName' and
-- the 'ScoreThreshold' in an 'MLModel' without creating a new 'MLModel'.
--
-- 'CreateMLModel' is an asynchronous operation. In response to 'CreateMLModel',
-- Amazon Machine Learning (Amazon ML) immediately returns and sets the 'MLModel'
-- status to 'PENDING'. After the 'MLModel' is created and ready for use, Amazon ML
-- sets the status to 'COMPLETED'.
--
-- You can use the 'GetMLModel' operation to check progress of the 'MLModel' during
-- the creation operation.
--
-- 'CreateMLModel' requires a 'DataSource' with computed statistics, which can be
-- created by setting 'ComputeStatistics' to 'true' in 'CreateDataSourceFromRDS', 'CreateDataSourceFromS3', or 'CreateDataSourceFromRedshift' operations.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateMLModel.html>
module Network.AWS.MachineLearning.CreateMLModel
    (
    -- * Request
      CreateMLModel
    -- ** Request constructor
    , createMLModel
    -- ** Request lenses
    , cmlmMLModelId
    , cmlmMLModelName
    , cmlmMLModelType
    , cmlmParameters
    , cmlmRecipe
    , cmlmRecipeUri
    , cmlmTrainingDataSourceId

    -- * Response
    , CreateMLModelResponse
    -- ** Response constructor
    , createMLModelResponse
    -- ** Response lenses
    , cmlmrMLModelId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data CreateMLModel = CreateMLModel
    { _cmlmMLModelId            :: Text
    , _cmlmMLModelName          :: Maybe Text
    , _cmlmMLModelType          :: MLModelType
    , _cmlmParameters           :: Map Text Text
    , _cmlmRecipe               :: Maybe Text
    , _cmlmRecipeUri            :: Maybe Text
    , _cmlmTrainingDataSourceId :: Text
    } deriving (Eq, Read, Show)

-- | 'CreateMLModel' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmlmMLModelId' @::@ 'Text'
--
-- * 'cmlmMLModelName' @::@ 'Maybe' 'Text'
--
-- * 'cmlmMLModelType' @::@ 'MLModelType'
--
-- * 'cmlmParameters' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'cmlmRecipe' @::@ 'Maybe' 'Text'
--
-- * 'cmlmRecipeUri' @::@ 'Maybe' 'Text'
--
-- * 'cmlmTrainingDataSourceId' @::@ 'Text'
--
createMLModel :: Text -- ^ 'cmlmMLModelId'
              -> MLModelType -- ^ 'cmlmMLModelType'
              -> Text -- ^ 'cmlmTrainingDataSourceId'
              -> CreateMLModel
createMLModel p1 p2 p3 = CreateMLModel
    { _cmlmMLModelId            = p1
    , _cmlmMLModelType          = p2
    , _cmlmTrainingDataSourceId = p3
    , _cmlmMLModelName          = Nothing
    , _cmlmParameters           = mempty
    , _cmlmRecipe               = Nothing
    , _cmlmRecipeUri            = Nothing
    }

-- | A user-supplied ID that uniquely identifies the 'MLModel'.
cmlmMLModelId :: Lens' CreateMLModel Text
cmlmMLModelId = lens _cmlmMLModelId (\s a -> s { _cmlmMLModelId = a })

-- | A user-supplied name or description of the 'MLModel'.
cmlmMLModelName :: Lens' CreateMLModel (Maybe Text)
cmlmMLModelName = lens _cmlmMLModelName (\s a -> s { _cmlmMLModelName = a })

-- | The category of supervised learning that this 'MLModel' will address. Choose
-- from the following types:
--
-- Choose 'REGRESSION' if the 'MLModel' will be used to predict a numeric value. Choose
-- 'BINARY' if the 'MLModel' result has two possible values. Choose 'MULTICLASS' if
-- the 'MLModel' result has a limited number of values.    For more information,
-- see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide>.
cmlmMLModelType :: Lens' CreateMLModel MLModelType
cmlmMLModelType = lens _cmlmMLModelType (\s a -> s { _cmlmMLModelType = a })

-- | A list of the training parameters in the 'MLModel'. The list is implemented as
-- a map of key/value pairs.
--
-- The following is the current set of training parameters:
--
-- 'sgd.l1RegularizationAmount' - Coefficient regularization L1 norm. It
-- controls overfitting the data by penalizing large coefficients. This tends to
-- drive coefficients to zero, resulting in sparse feature set. If you use this
-- parameter, start by specifying a small value such as 1.0E-08.
--
-- The value is a double that ranges from 0 to MAX_DOUBLE. The default is not
-- to use L1 normalization. The parameter cannot be used when 'L2' is specified.
-- Use this parameter sparingly.
--
-- 'sgd.l2RegularizationAmount' - Coefficient regularization L2 norm. It
-- controls overfitting the data by penalizing large coefficients. This tends to
-- drive coefficients to small, nonzero values. If you use this parameter, start
-- by specifying a small value such as 1.0E-08.
--
-- The valuseis a double that ranges from 0 to MAX_DOUBLE. The default is not
-- to use L2 normalization. This cannot be used when 'L1' is specified. Use this
-- parameter sparingly.
--
-- 'sgd.maxPasses' - Number of times that the training process traverses the
-- observations to build the 'MLModel'. The value is an integer that ranges from 1
-- to 10000. The default value is 10.
--
-- 'sgd.maxMLModelSizeInBytes' - Maximum allowed size of the model. Depending on
-- the input data, the size of the model might affect its performance.
--
-- The value is an integer that ranges from 100000 to 2147483648. The default
-- value is 33554432.
--
--
cmlmParameters :: Lens' CreateMLModel (HashMap Text Text)
cmlmParameters = lens _cmlmParameters (\s a -> s { _cmlmParameters = a }) . _Map

-- | The data recipe for creating 'MLModel'. You must specify either the recipe or
-- its URI. If you don’t specify a recipe or its URI, Amazon ML creates a
-- default.
cmlmRecipe :: Lens' CreateMLModel (Maybe Text)
cmlmRecipe = lens _cmlmRecipe (\s a -> s { _cmlmRecipe = a })

-- | The Amazon Simple Storage Service (Amazon S3) location and file name that
-- contains the 'MLModel' recipe. You must specify either the recipe or its URI.
-- If you don’t specify a recipe or its URI, Amazon ML creates a default.
cmlmRecipeUri :: Lens' CreateMLModel (Maybe Text)
cmlmRecipeUri = lens _cmlmRecipeUri (\s a -> s { _cmlmRecipeUri = a })

-- | The 'DataSource' that points to the training data.
cmlmTrainingDataSourceId :: Lens' CreateMLModel Text
cmlmTrainingDataSourceId =
    lens _cmlmTrainingDataSourceId
        (\s a -> s { _cmlmTrainingDataSourceId = a })

newtype CreateMLModelResponse = CreateMLModelResponse
    { _cmlmrMLModelId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'CreateMLModelResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmlmrMLModelId' @::@ 'Maybe' 'Text'
--
createMLModelResponse :: CreateMLModelResponse
createMLModelResponse = CreateMLModelResponse
    { _cmlmrMLModelId = Nothing
    }

-- | A user-supplied ID that uniquely identifies the 'MLModel'. This value should be
-- identical to the value of the 'MLModelId' in the request.
cmlmrMLModelId :: Lens' CreateMLModelResponse (Maybe Text)
cmlmrMLModelId = lens _cmlmrMLModelId (\s a -> s { _cmlmrMLModelId = a })

instance ToPath CreateMLModel where
    toPath = const "/"

instance ToQuery CreateMLModel where
    toQuery = const mempty

instance ToHeaders CreateMLModel

instance ToJSON CreateMLModel where
    toJSON CreateMLModel{..} = object
        [ "MLModelId"            .= _cmlmMLModelId
        , "MLModelName"          .= _cmlmMLModelName
        , "MLModelType"          .= _cmlmMLModelType
        , "Parameters"           .= _cmlmParameters
        , "TrainingDataSourceId" .= _cmlmTrainingDataSourceId
        , "Recipe"               .= _cmlmRecipe
        , "RecipeUri"            .= _cmlmRecipeUri
        ]

instance AWSRequest CreateMLModel where
    type Sv CreateMLModel = MachineLearning
    type Rs CreateMLModel = CreateMLModelResponse

    request  = post "CreateMLModel"
    response = jsonResponse

instance FromJSON CreateMLModelResponse where
    parseJSON = withObject "CreateMLModelResponse" $ \o -> CreateMLModelResponse
        <$> o .:? "MLModelId"
