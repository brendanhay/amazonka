{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The source of the trial.
--
--
--
-- /See:/ 'trialSource' smart constructor.
data TrialSource = TrialSource'
  { _tsSourceType :: !(Maybe Text),
    _tsSourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrialSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsSourceType' - The source job type.
--
-- * 'tsSourceARN' - The Amazon Resource Name (ARN) of the source.
trialSource ::
  -- | 'tsSourceARN'
  Text ->
  TrialSource
trialSource pSourceARN_ =
  TrialSource' {_tsSourceType = Nothing, _tsSourceARN = pSourceARN_}

-- | The source job type.
tsSourceType :: Lens' TrialSource (Maybe Text)
tsSourceType = lens _tsSourceType (\s a -> s {_tsSourceType = a})

-- | The Amazon Resource Name (ARN) of the source.
tsSourceARN :: Lens' TrialSource Text
tsSourceARN = lens _tsSourceARN (\s a -> s {_tsSourceARN = a})

instance FromJSON TrialSource where
  parseJSON =
    withObject
      "TrialSource"
      ( \x ->
          TrialSource' <$> (x .:? "SourceType") <*> (x .: "SourceArn")
      )

instance Hashable TrialSource

instance NFData TrialSource
