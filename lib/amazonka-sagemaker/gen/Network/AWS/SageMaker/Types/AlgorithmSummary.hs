{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AlgorithmStatus

-- | Provides summary information about an algorithm.
--
--
--
-- /See:/ 'algorithmSummary' smart constructor.
data AlgorithmSummary = AlgorithmSummary'
  { _aAlgorithmDescription ::
      !(Maybe Text),
    _aAlgorithmName :: !Text,
    _aAlgorithmARN :: !Text,
    _aCreationTime :: !POSIX,
    _aAlgorithmStatus :: !AlgorithmStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AlgorithmSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAlgorithmDescription' - A brief description of the algorithm.
--
-- * 'aAlgorithmName' - The name of the algorithm that is described by the summary.
--
-- * 'aAlgorithmARN' - The Amazon Resource Name (ARN) of the algorithm.
--
-- * 'aCreationTime' - A timestamp that shows when the algorithm was created.
--
-- * 'aAlgorithmStatus' - The overall status of the algorithm.
algorithmSummary ::
  -- | 'aAlgorithmName'
  Text ->
  -- | 'aAlgorithmARN'
  Text ->
  -- | 'aCreationTime'
  UTCTime ->
  -- | 'aAlgorithmStatus'
  AlgorithmStatus ->
  AlgorithmSummary
algorithmSummary
  pAlgorithmName_
  pAlgorithmARN_
  pCreationTime_
  pAlgorithmStatus_ =
    AlgorithmSummary'
      { _aAlgorithmDescription = Nothing,
        _aAlgorithmName = pAlgorithmName_,
        _aAlgorithmARN = pAlgorithmARN_,
        _aCreationTime = _Time # pCreationTime_,
        _aAlgorithmStatus = pAlgorithmStatus_
      }

-- | A brief description of the algorithm.
aAlgorithmDescription :: Lens' AlgorithmSummary (Maybe Text)
aAlgorithmDescription = lens _aAlgorithmDescription (\s a -> s {_aAlgorithmDescription = a})

-- | The name of the algorithm that is described by the summary.
aAlgorithmName :: Lens' AlgorithmSummary Text
aAlgorithmName = lens _aAlgorithmName (\s a -> s {_aAlgorithmName = a})

-- | The Amazon Resource Name (ARN) of the algorithm.
aAlgorithmARN :: Lens' AlgorithmSummary Text
aAlgorithmARN = lens _aAlgorithmARN (\s a -> s {_aAlgorithmARN = a})

-- | A timestamp that shows when the algorithm was created.
aCreationTime :: Lens' AlgorithmSummary UTCTime
aCreationTime = lens _aCreationTime (\s a -> s {_aCreationTime = a}) . _Time

-- | The overall status of the algorithm.
aAlgorithmStatus :: Lens' AlgorithmSummary AlgorithmStatus
aAlgorithmStatus = lens _aAlgorithmStatus (\s a -> s {_aAlgorithmStatus = a})

instance FromJSON AlgorithmSummary where
  parseJSON =
    withObject
      "AlgorithmSummary"
      ( \x ->
          AlgorithmSummary'
            <$> (x .:? "AlgorithmDescription")
            <*> (x .: "AlgorithmName")
            <*> (x .: "AlgorithmArn")
            <*> (x .: "CreationTime")
            <*> (x .: "AlgorithmStatus")
      )

instance Hashable AlgorithmSummary

instance NFData AlgorithmSummary
