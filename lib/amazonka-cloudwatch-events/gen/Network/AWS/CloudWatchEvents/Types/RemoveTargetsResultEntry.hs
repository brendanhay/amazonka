{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a target that failed to be removed from a rule.
--
--
--
-- /See:/ 'removeTargetsResultEntry' smart constructor.
data RemoveTargetsResultEntry = RemoveTargetsResultEntry'
  { _rtreTargetId ::
      !(Maybe Text),
    _rtreErrorCode :: !(Maybe Text),
    _rtreErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveTargetsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtreTargetId' - The ID of the target.
--
-- * 'rtreErrorCode' - The error code that indicates why the target removal failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
--
-- * 'rtreErrorMessage' - The error message that explains why the target removal failed.
removeTargetsResultEntry ::
  RemoveTargetsResultEntry
removeTargetsResultEntry =
  RemoveTargetsResultEntry'
    { _rtreTargetId = Nothing,
      _rtreErrorCode = Nothing,
      _rtreErrorMessage = Nothing
    }

-- | The ID of the target.
rtreTargetId :: Lens' RemoveTargetsResultEntry (Maybe Text)
rtreTargetId = lens _rtreTargetId (\s a -> s {_rtreTargetId = a})

-- | The error code that indicates why the target removal failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
rtreErrorCode :: Lens' RemoveTargetsResultEntry (Maybe Text)
rtreErrorCode = lens _rtreErrorCode (\s a -> s {_rtreErrorCode = a})

-- | The error message that explains why the target removal failed.
rtreErrorMessage :: Lens' RemoveTargetsResultEntry (Maybe Text)
rtreErrorMessage = lens _rtreErrorMessage (\s a -> s {_rtreErrorMessage = a})

instance FromJSON RemoveTargetsResultEntry where
  parseJSON =
    withObject
      "RemoveTargetsResultEntry"
      ( \x ->
          RemoveTargetsResultEntry'
            <$> (x .:? "TargetId")
            <*> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable RemoveTargetsResultEntry

instance NFData RemoveTargetsResultEntry
