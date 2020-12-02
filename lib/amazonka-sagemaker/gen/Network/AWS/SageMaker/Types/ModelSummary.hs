{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides summary information about a model.
--
--
--
-- /See:/ 'modelSummary' smart constructor.
data ModelSummary = ModelSummary'
  { _msModelName :: !Text,
    _msModelARN :: !Text,
    _msCreationTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModelSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msModelName' - The name of the model that you want a summary for.
--
-- * 'msModelARN' - The Amazon Resource Name (ARN) of the model.
--
-- * 'msCreationTime' - A timestamp that indicates when the model was created.
modelSummary ::
  -- | 'msModelName'
  Text ->
  -- | 'msModelARN'
  Text ->
  -- | 'msCreationTime'
  UTCTime ->
  ModelSummary
modelSummary pModelName_ pModelARN_ pCreationTime_ =
  ModelSummary'
    { _msModelName = pModelName_,
      _msModelARN = pModelARN_,
      _msCreationTime = _Time # pCreationTime_
    }

-- | The name of the model that you want a summary for.
msModelName :: Lens' ModelSummary Text
msModelName = lens _msModelName (\s a -> s {_msModelName = a})

-- | The Amazon Resource Name (ARN) of the model.
msModelARN :: Lens' ModelSummary Text
msModelARN = lens _msModelARN (\s a -> s {_msModelARN = a})

-- | A timestamp that indicates when the model was created.
msCreationTime :: Lens' ModelSummary UTCTime
msCreationTime = lens _msCreationTime (\s a -> s {_msCreationTime = a}) . _Time

instance FromJSON ModelSummary where
  parseJSON =
    withObject
      "ModelSummary"
      ( \x ->
          ModelSummary'
            <$> (x .: "ModelName") <*> (x .: "ModelArn") <*> (x .: "CreationTime")
      )

instance Hashable ModelSummary

instance NFData ModelSummary
