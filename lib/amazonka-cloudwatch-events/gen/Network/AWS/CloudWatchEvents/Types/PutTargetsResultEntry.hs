{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a target that failed to be added to a rule.
--
--
--
-- /See:/ 'putTargetsResultEntry' smart constructor.
data PutTargetsResultEntry = PutTargetsResultEntry'
  { _ptreTargetId ::
      !(Maybe Text),
    _ptreErrorCode :: !(Maybe Text),
    _ptreErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutTargetsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptreTargetId' - The ID of the target.
--
-- * 'ptreErrorCode' - The error code that indicates why the target addition failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
--
-- * 'ptreErrorMessage' - The error message that explains why the target addition failed.
putTargetsResultEntry ::
  PutTargetsResultEntry
putTargetsResultEntry =
  PutTargetsResultEntry'
    { _ptreTargetId = Nothing,
      _ptreErrorCode = Nothing,
      _ptreErrorMessage = Nothing
    }

-- | The ID of the target.
ptreTargetId :: Lens' PutTargetsResultEntry (Maybe Text)
ptreTargetId = lens _ptreTargetId (\s a -> s {_ptreTargetId = a})

-- | The error code that indicates why the target addition failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
ptreErrorCode :: Lens' PutTargetsResultEntry (Maybe Text)
ptreErrorCode = lens _ptreErrorCode (\s a -> s {_ptreErrorCode = a})

-- | The error message that explains why the target addition failed.
ptreErrorMessage :: Lens' PutTargetsResultEntry (Maybe Text)
ptreErrorMessage = lens _ptreErrorMessage (\s a -> s {_ptreErrorMessage = a})

instance FromJSON PutTargetsResultEntry where
  parseJSON =
    withObject
      "PutTargetsResultEntry"
      ( \x ->
          PutTargetsResultEntry'
            <$> (x .:? "TargetId")
            <*> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable PutTargetsResultEntry

instance NFData PutTargetsResultEntry
