{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.EvaluationResultIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.EvaluationResultIdentifier where

import Network.AWS.Config.Types.EvaluationResultQualifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Uniquely identifies an evaluation result.
--
--
--
-- /See:/ 'evaluationResultIdentifier' smart constructor.
data EvaluationResultIdentifier = EvaluationResultIdentifier'
  { _eriEvaluationResultQualifier ::
      !(Maybe EvaluationResultQualifier),
    _eriOrderingTimestamp ::
      !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EvaluationResultIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eriEvaluationResultQualifier' - Identifies an AWS Config rule used to evaluate an AWS resource, and provides the type and ID of the evaluated resource.
--
-- * 'eriOrderingTimestamp' - The time of the event that triggered the evaluation of your AWS resources. The time can indicate when AWS Config delivered a configuration item change notification, or it can indicate when AWS Config delivered the configuration snapshot, depending on which event triggered the evaluation.
evaluationResultIdentifier ::
  EvaluationResultIdentifier
evaluationResultIdentifier =
  EvaluationResultIdentifier'
    { _eriEvaluationResultQualifier =
        Nothing,
      _eriOrderingTimestamp = Nothing
    }

-- | Identifies an AWS Config rule used to evaluate an AWS resource, and provides the type and ID of the evaluated resource.
eriEvaluationResultQualifier :: Lens' EvaluationResultIdentifier (Maybe EvaluationResultQualifier)
eriEvaluationResultQualifier = lens _eriEvaluationResultQualifier (\s a -> s {_eriEvaluationResultQualifier = a})

-- | The time of the event that triggered the evaluation of your AWS resources. The time can indicate when AWS Config delivered a configuration item change notification, or it can indicate when AWS Config delivered the configuration snapshot, depending on which event triggered the evaluation.
eriOrderingTimestamp :: Lens' EvaluationResultIdentifier (Maybe UTCTime)
eriOrderingTimestamp = lens _eriOrderingTimestamp (\s a -> s {_eriOrderingTimestamp = a}) . mapping _Time

instance FromJSON EvaluationResultIdentifier where
  parseJSON =
    withObject
      "EvaluationResultIdentifier"
      ( \x ->
          EvaluationResultIdentifier'
            <$> (x .:? "EvaluationResultQualifier")
            <*> (x .:? "OrderingTimestamp")
      )

instance Hashable EvaluationResultIdentifier

instance NFData EvaluationResultIdentifier
