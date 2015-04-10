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

-- Module      : Network.AWS.MachineLearning.DeleteMLModel
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

-- | Assigns the DELETED status to an 'MLModel', rendering it unusable.
--
-- After using the 'DeleteMLModel' operation, you can use the 'GetMLModel'
-- operation to verify that the status of the 'MLModel' changed to DELETED.
--
-- Caution The result of the 'DeleteMLModel' operation is irreversible.
--
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteMLModel.html>
module Network.AWS.MachineLearning.DeleteMLModel
    (
    -- * Request
      DeleteMLModel
    -- ** Request constructor
    , deleteMLModel
    -- ** Request lenses
    , dmlmMLModelId

    -- * Response
    , DeleteMLModelResponse
    -- ** Response constructor
    , deleteMLModelResponse
    -- ** Response lenses
    , dmlmrMLModelId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

newtype DeleteMLModel = DeleteMLModel
    { _dmlmMLModelId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteMLModel' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmlmMLModelId' @::@ 'Text'
--
deleteMLModel :: Text -- ^ 'dmlmMLModelId'
              -> DeleteMLModel
deleteMLModel p1 = DeleteMLModel
    { _dmlmMLModelId = p1
    }

-- | A user-supplied ID that uniquely identifies the 'MLModel'.
dmlmMLModelId :: Lens' DeleteMLModel Text
dmlmMLModelId = lens _dmlmMLModelId (\s a -> s { _dmlmMLModelId = a })

newtype DeleteMLModelResponse = DeleteMLModelResponse
    { _dmlmrMLModelId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'DeleteMLModelResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmlmrMLModelId' @::@ 'Maybe' 'Text'
--
deleteMLModelResponse :: DeleteMLModelResponse
deleteMLModelResponse = DeleteMLModelResponse
    { _dmlmrMLModelId = Nothing
    }

-- | A user-supplied ID that uniquely identifies the 'MLModel'. This value should be
-- identical to the value of the 'MLModelID' in the request.
dmlmrMLModelId :: Lens' DeleteMLModelResponse (Maybe Text)
dmlmrMLModelId = lens _dmlmrMLModelId (\s a -> s { _dmlmrMLModelId = a })

instance ToPath DeleteMLModel where
    toPath = const "/"

instance ToQuery DeleteMLModel where
    toQuery = const mempty

instance ToHeaders DeleteMLModel

instance ToJSON DeleteMLModel where
    toJSON DeleteMLModel{..} = object
        [ "MLModelId" .= _dmlmMLModelId
        ]

instance AWSRequest DeleteMLModel where
    type Sv DeleteMLModel = MachineLearning
    type Rs DeleteMLModel = DeleteMLModelResponse

    request  = post "DeleteMLModel"
    response = jsonResponse

instance FromJSON DeleteMLModelResponse where
    parseJSON = withObject "DeleteMLModelResponse" $ \o -> DeleteMLModelResponse
        <$> o .:? "MLModelId"
