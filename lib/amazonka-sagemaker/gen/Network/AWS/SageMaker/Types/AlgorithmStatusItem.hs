{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmStatusItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.DetailedAlgorithmStatus

-- | Represents the overall status of an algorithm.
--
--
--
-- /See:/ 'algorithmStatusItem' smart constructor.
data AlgorithmStatusItem = AlgorithmStatusItem'
  { _asiFailureReason ::
      !(Maybe Text),
    _asiName :: !Text,
    _asiStatus :: !DetailedAlgorithmStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AlgorithmStatusItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asiFailureReason' - if the overall status is @Failed@ , the reason for the failure.
--
-- * 'asiName' - The name of the algorithm for which the overall status is being reported.
--
-- * 'asiStatus' - The current status.
algorithmStatusItem ::
  -- | 'asiName'
  Text ->
  -- | 'asiStatus'
  DetailedAlgorithmStatus ->
  AlgorithmStatusItem
algorithmStatusItem pName_ pStatus_ =
  AlgorithmStatusItem'
    { _asiFailureReason = Nothing,
      _asiName = pName_,
      _asiStatus = pStatus_
    }

-- | if the overall status is @Failed@ , the reason for the failure.
asiFailureReason :: Lens' AlgorithmStatusItem (Maybe Text)
asiFailureReason = lens _asiFailureReason (\s a -> s {_asiFailureReason = a})

-- | The name of the algorithm for which the overall status is being reported.
asiName :: Lens' AlgorithmStatusItem Text
asiName = lens _asiName (\s a -> s {_asiName = a})

-- | The current status.
asiStatus :: Lens' AlgorithmStatusItem DetailedAlgorithmStatus
asiStatus = lens _asiStatus (\s a -> s {_asiStatus = a})

instance FromJSON AlgorithmStatusItem where
  parseJSON =
    withObject
      "AlgorithmStatusItem"
      ( \x ->
          AlgorithmStatusItem'
            <$> (x .:? "FailureReason") <*> (x .: "Name") <*> (x .: "Status")
      )

instance Hashable AlgorithmStatusItem

instance NFData AlgorithmStatusItem
