{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.FailedResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.FailedResource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A resource that failed to be added to or removed from a group.
--
--
--
-- /See:/ 'failedResource' smart constructor.
data FailedResource = FailedResource'
  { _frResourceARN ::
      !(Maybe Text),
    _frErrorCode :: !(Maybe Text),
    _frErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailedResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frResourceARN' - The ARN of the resource that failed to be added or removed.
--
-- * 'frErrorCode' - The error code associated with the failure.
--
-- * 'frErrorMessage' - The error message text associated with the failure.
failedResource ::
  FailedResource
failedResource =
  FailedResource'
    { _frResourceARN = Nothing,
      _frErrorCode = Nothing,
      _frErrorMessage = Nothing
    }

-- | The ARN of the resource that failed to be added or removed.
frResourceARN :: Lens' FailedResource (Maybe Text)
frResourceARN = lens _frResourceARN (\s a -> s {_frResourceARN = a})

-- | The error code associated with the failure.
frErrorCode :: Lens' FailedResource (Maybe Text)
frErrorCode = lens _frErrorCode (\s a -> s {_frErrorCode = a})

-- | The error message text associated with the failure.
frErrorMessage :: Lens' FailedResource (Maybe Text)
frErrorMessage = lens _frErrorMessage (\s a -> s {_frErrorMessage = a})

instance FromJSON FailedResource where
  parseJSON =
    withObject
      "FailedResource"
      ( \x ->
          FailedResource'
            <$> (x .:? "ResourceArn")
            <*> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable FailedResource

instance NFData FailedResource
