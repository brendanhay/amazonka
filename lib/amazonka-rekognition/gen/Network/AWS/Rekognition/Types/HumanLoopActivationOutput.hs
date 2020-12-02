{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.HumanLoopActivationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.HumanLoopActivationOutput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Shows the results of the human in the loop evaluation. If there is no HumanLoopArn, the input did not trigger human review.
--
--
--
-- /See:/ 'humanLoopActivationOutput' smart constructor.
data HumanLoopActivationOutput = HumanLoopActivationOutput'
  { _hlaoHumanLoopActivationReasons ::
      !(Maybe (List1 Text)),
    _hlaoHumanLoopARN :: !(Maybe Text),
    _hlaoHumanLoopActivationConditionsEvaluationResults ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HumanLoopActivationOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlaoHumanLoopActivationReasons' - Shows if and why human review was needed.
--
-- * 'hlaoHumanLoopARN' - The Amazon Resource Name (ARN) of the HumanLoop created.
--
-- * 'hlaoHumanLoopActivationConditionsEvaluationResults' - Shows the result of condition evaluations, including those conditions which activated a human review.
humanLoopActivationOutput ::
  HumanLoopActivationOutput
humanLoopActivationOutput =
  HumanLoopActivationOutput'
    { _hlaoHumanLoopActivationReasons =
        Nothing,
      _hlaoHumanLoopARN = Nothing,
      _hlaoHumanLoopActivationConditionsEvaluationResults = Nothing
    }

-- | Shows if and why human review was needed.
hlaoHumanLoopActivationReasons :: Lens' HumanLoopActivationOutput (Maybe (NonEmpty Text))
hlaoHumanLoopActivationReasons = lens _hlaoHumanLoopActivationReasons (\s a -> s {_hlaoHumanLoopActivationReasons = a}) . mapping _List1

-- | The Amazon Resource Name (ARN) of the HumanLoop created.
hlaoHumanLoopARN :: Lens' HumanLoopActivationOutput (Maybe Text)
hlaoHumanLoopARN = lens _hlaoHumanLoopARN (\s a -> s {_hlaoHumanLoopARN = a})

-- | Shows the result of condition evaluations, including those conditions which activated a human review.
hlaoHumanLoopActivationConditionsEvaluationResults :: Lens' HumanLoopActivationOutput (Maybe Text)
hlaoHumanLoopActivationConditionsEvaluationResults = lens _hlaoHumanLoopActivationConditionsEvaluationResults (\s a -> s {_hlaoHumanLoopActivationConditionsEvaluationResults = a})

instance FromJSON HumanLoopActivationOutput where
  parseJSON =
    withObject
      "HumanLoopActivationOutput"
      ( \x ->
          HumanLoopActivationOutput'
            <$> (x .:? "HumanLoopActivationReasons")
            <*> (x .:? "HumanLoopArn")
            <*> (x .:? "HumanLoopActivationConditionsEvaluationResults")
      )

instance Hashable HumanLoopActivationOutput

instance NFData HumanLoopActivationOutput
