{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a scheduled action that could not be created, updated, or deleted.
--
--
--
-- /See:/ 'failedScheduledUpdateGroupActionRequest' smart constructor.
data FailedScheduledUpdateGroupActionRequest = FailedScheduledUpdateGroupActionRequest'
  { _fsugarErrorCode ::
      !( Maybe
           Text
       ),
    _fsugarErrorMessage ::
      !( Maybe
           Text
       ),
    _fsugarScheduledActionName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailedScheduledUpdateGroupActionRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsugarErrorCode' - The error code.
--
-- * 'fsugarErrorMessage' - The error message accompanying the error code.
--
-- * 'fsugarScheduledActionName' - The name of the scheduled action.
failedScheduledUpdateGroupActionRequest ::
  -- | 'fsugarScheduledActionName'
  Text ->
  FailedScheduledUpdateGroupActionRequest
failedScheduledUpdateGroupActionRequest pScheduledActionName_ =
  FailedScheduledUpdateGroupActionRequest'
    { _fsugarErrorCode =
        Nothing,
      _fsugarErrorMessage = Nothing,
      _fsugarScheduledActionName = pScheduledActionName_
    }

-- | The error code.
fsugarErrorCode :: Lens' FailedScheduledUpdateGroupActionRequest (Maybe Text)
fsugarErrorCode = lens _fsugarErrorCode (\s a -> s {_fsugarErrorCode = a})

-- | The error message accompanying the error code.
fsugarErrorMessage :: Lens' FailedScheduledUpdateGroupActionRequest (Maybe Text)
fsugarErrorMessage = lens _fsugarErrorMessage (\s a -> s {_fsugarErrorMessage = a})

-- | The name of the scheduled action.
fsugarScheduledActionName :: Lens' FailedScheduledUpdateGroupActionRequest Text
fsugarScheduledActionName = lens _fsugarScheduledActionName (\s a -> s {_fsugarScheduledActionName = a})

instance FromXML FailedScheduledUpdateGroupActionRequest where
  parseXML x =
    FailedScheduledUpdateGroupActionRequest'
      <$> (x .@? "ErrorCode")
      <*> (x .@? "ErrorMessage")
      <*> (x .@ "ScheduledActionName")

instance Hashable FailedScheduledUpdateGroupActionRequest

instance NFData FailedScheduledUpdateGroupActionRequest
