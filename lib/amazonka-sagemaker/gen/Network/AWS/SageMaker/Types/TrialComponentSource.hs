{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon Resource Name (ARN) and job type of the source of a trial component.
--
--
--
-- /See:/ 'trialComponentSource' smart constructor.
data TrialComponentSource = TrialComponentSource'
  { _tcsSourceType ::
      !(Maybe Text),
    _tcsSourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrialComponentSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcsSourceType' - The source job type.
--
-- * 'tcsSourceARN' - The source ARN.
trialComponentSource ::
  -- | 'tcsSourceARN'
  Text ->
  TrialComponentSource
trialComponentSource pSourceARN_ =
  TrialComponentSource'
    { _tcsSourceType = Nothing,
      _tcsSourceARN = pSourceARN_
    }

-- | The source job type.
tcsSourceType :: Lens' TrialComponentSource (Maybe Text)
tcsSourceType = lens _tcsSourceType (\s a -> s {_tcsSourceType = a})

-- | The source ARN.
tcsSourceARN :: Lens' TrialComponentSource Text
tcsSourceARN = lens _tcsSourceARN (\s a -> s {_tcsSourceARN = a})

instance FromJSON TrialComponentSource where
  parseJSON =
    withObject
      "TrialComponentSource"
      ( \x ->
          TrialComponentSource'
            <$> (x .:? "SourceType") <*> (x .: "SourceArn")
      )

instance Hashable TrialComponentSource

instance NFData TrialComponentSource
