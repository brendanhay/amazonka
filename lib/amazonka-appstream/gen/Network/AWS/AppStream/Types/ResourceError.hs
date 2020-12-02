{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ResourceError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ResourceError where

import Network.AWS.AppStream.Types.FleetErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a resource error.
--
--
--
-- /See:/ 'resourceError' smart constructor.
data ResourceError = ResourceError'
  { _reErrorCode ::
      !(Maybe FleetErrorCode),
    _reErrorMessage :: !(Maybe Text),
    _reErrorTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reErrorCode' - The error code.
--
-- * 'reErrorMessage' - The error message.
--
-- * 'reErrorTimestamp' - The time the error occurred.
resourceError ::
  ResourceError
resourceError =
  ResourceError'
    { _reErrorCode = Nothing,
      _reErrorMessage = Nothing,
      _reErrorTimestamp = Nothing
    }

-- | The error code.
reErrorCode :: Lens' ResourceError (Maybe FleetErrorCode)
reErrorCode = lens _reErrorCode (\s a -> s {_reErrorCode = a})

-- | The error message.
reErrorMessage :: Lens' ResourceError (Maybe Text)
reErrorMessage = lens _reErrorMessage (\s a -> s {_reErrorMessage = a})

-- | The time the error occurred.
reErrorTimestamp :: Lens' ResourceError (Maybe UTCTime)
reErrorTimestamp = lens _reErrorTimestamp (\s a -> s {_reErrorTimestamp = a}) . mapping _Time

instance FromJSON ResourceError where
  parseJSON =
    withObject
      "ResourceError"
      ( \x ->
          ResourceError'
            <$> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
            <*> (x .:? "ErrorTimestamp")
      )

instance Hashable ResourceError

instance NFData ResourceError
