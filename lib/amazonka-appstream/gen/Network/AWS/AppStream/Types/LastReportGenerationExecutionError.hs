{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.LastReportGenerationExecutionError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.LastReportGenerationExecutionError where

import Network.AWS.AppStream.Types.UsageReportExecutionErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the error that is returned when a usage report can't be generated.
--
--
--
-- /See:/ 'lastReportGenerationExecutionError' smart constructor.
data LastReportGenerationExecutionError = LastReportGenerationExecutionError'
  { _lrgeeErrorCode ::
      !( Maybe
           UsageReportExecutionErrorCode
       ),
    _lrgeeErrorMessage ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LastReportGenerationExecutionError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrgeeErrorCode' - The error code for the error that is returned when a usage report can't be generated.
--
-- * 'lrgeeErrorMessage' - The error message for the error that is returned when a usage report can't be generated.
lastReportGenerationExecutionError ::
  LastReportGenerationExecutionError
lastReportGenerationExecutionError =
  LastReportGenerationExecutionError'
    { _lrgeeErrorCode = Nothing,
      _lrgeeErrorMessage = Nothing
    }

-- | The error code for the error that is returned when a usage report can't be generated.
lrgeeErrorCode :: Lens' LastReportGenerationExecutionError (Maybe UsageReportExecutionErrorCode)
lrgeeErrorCode = lens _lrgeeErrorCode (\s a -> s {_lrgeeErrorCode = a})

-- | The error message for the error that is returned when a usage report can't be generated.
lrgeeErrorMessage :: Lens' LastReportGenerationExecutionError (Maybe Text)
lrgeeErrorMessage = lens _lrgeeErrorMessage (\s a -> s {_lrgeeErrorMessage = a})

instance FromJSON LastReportGenerationExecutionError where
  parseJSON =
    withObject
      "LastReportGenerationExecutionError"
      ( \x ->
          LastReportGenerationExecutionError'
            <$> (x .:? "ErrorCode") <*> (x .:? "ErrorMessage")
      )

instance Hashable LastReportGenerationExecutionError

instance NFData LastReportGenerationExecutionError
