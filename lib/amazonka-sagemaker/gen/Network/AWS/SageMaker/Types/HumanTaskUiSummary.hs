{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanTaskUiSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanTaskUiSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Container for human task user interface information.
--
--
--
-- /See:/ 'humanTaskUiSummary' smart constructor.
data HumanTaskUiSummary = HumanTaskUiSummary'
  { _htusHumanTaskUiName ::
      !Text,
    _htusHumanTaskUiARN :: !Text,
    _htusCreationTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HumanTaskUiSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'htusHumanTaskUiName' - The name of the human task user interface.
--
-- * 'htusHumanTaskUiARN' - The Amazon Resource Name (ARN) of the human task user interface.
--
-- * 'htusCreationTime' - A timestamp when SageMaker created the human task user interface.
humanTaskUiSummary ::
  -- | 'htusHumanTaskUiName'
  Text ->
  -- | 'htusHumanTaskUiARN'
  Text ->
  -- | 'htusCreationTime'
  UTCTime ->
  HumanTaskUiSummary
humanTaskUiSummary
  pHumanTaskUiName_
  pHumanTaskUiARN_
  pCreationTime_ =
    HumanTaskUiSummary'
      { _htusHumanTaskUiName = pHumanTaskUiName_,
        _htusHumanTaskUiARN = pHumanTaskUiARN_,
        _htusCreationTime = _Time # pCreationTime_
      }

-- | The name of the human task user interface.
htusHumanTaskUiName :: Lens' HumanTaskUiSummary Text
htusHumanTaskUiName = lens _htusHumanTaskUiName (\s a -> s {_htusHumanTaskUiName = a})

-- | The Amazon Resource Name (ARN) of the human task user interface.
htusHumanTaskUiARN :: Lens' HumanTaskUiSummary Text
htusHumanTaskUiARN = lens _htusHumanTaskUiARN (\s a -> s {_htusHumanTaskUiARN = a})

-- | A timestamp when SageMaker created the human task user interface.
htusCreationTime :: Lens' HumanTaskUiSummary UTCTime
htusCreationTime = lens _htusCreationTime (\s a -> s {_htusCreationTime = a}) . _Time

instance FromJSON HumanTaskUiSummary where
  parseJSON =
    withObject
      "HumanTaskUiSummary"
      ( \x ->
          HumanTaskUiSummary'
            <$> (x .: "HumanTaskUiName")
            <*> (x .: "HumanTaskUiArn")
            <*> (x .: "CreationTime")
      )

instance Hashable HumanTaskUiSummary

instance NFData HumanTaskUiSummary
