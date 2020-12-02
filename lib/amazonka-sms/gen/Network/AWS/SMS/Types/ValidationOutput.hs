{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ValidationOutput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.AppValidationOutput
import Network.AWS.SMS.Types.ServerValidationOutput
import Network.AWS.SMS.Types.ValidationStatus

-- | Contains validation output.
--
--
--
-- /See:/ 'validationOutput' smart constructor.
data ValidationOutput = ValidationOutput'
  { _voStatus ::
      !(Maybe ValidationStatus),
    _voAppValidationOutput :: !(Maybe AppValidationOutput),
    _voLatestValidationTime :: !(Maybe POSIX),
    _voName :: !(Maybe Text),
    _voStatusMessage :: !(Maybe Text),
    _voValidationId :: !(Maybe Text),
    _voServerValidationOutput ::
      !(Maybe ServerValidationOutput)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidationOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'voStatus' - The status of the validation.
--
-- * 'voAppValidationOutput' - The output from validating an application.
--
-- * 'voLatestValidationTime' - The latest time that the validation was performed.
--
-- * 'voName' - The name of the validation.
--
-- * 'voStatusMessage' - The status message.
--
-- * 'voValidationId' - The ID of the validation.
--
-- * 'voServerValidationOutput' - The output from validation an instance.
validationOutput ::
  ValidationOutput
validationOutput =
  ValidationOutput'
    { _voStatus = Nothing,
      _voAppValidationOutput = Nothing,
      _voLatestValidationTime = Nothing,
      _voName = Nothing,
      _voStatusMessage = Nothing,
      _voValidationId = Nothing,
      _voServerValidationOutput = Nothing
    }

-- | The status of the validation.
voStatus :: Lens' ValidationOutput (Maybe ValidationStatus)
voStatus = lens _voStatus (\s a -> s {_voStatus = a})

-- | The output from validating an application.
voAppValidationOutput :: Lens' ValidationOutput (Maybe AppValidationOutput)
voAppValidationOutput = lens _voAppValidationOutput (\s a -> s {_voAppValidationOutput = a})

-- | The latest time that the validation was performed.
voLatestValidationTime :: Lens' ValidationOutput (Maybe UTCTime)
voLatestValidationTime = lens _voLatestValidationTime (\s a -> s {_voLatestValidationTime = a}) . mapping _Time

-- | The name of the validation.
voName :: Lens' ValidationOutput (Maybe Text)
voName = lens _voName (\s a -> s {_voName = a})

-- | The status message.
voStatusMessage :: Lens' ValidationOutput (Maybe Text)
voStatusMessage = lens _voStatusMessage (\s a -> s {_voStatusMessage = a})

-- | The ID of the validation.
voValidationId :: Lens' ValidationOutput (Maybe Text)
voValidationId = lens _voValidationId (\s a -> s {_voValidationId = a})

-- | The output from validation an instance.
voServerValidationOutput :: Lens' ValidationOutput (Maybe ServerValidationOutput)
voServerValidationOutput = lens _voServerValidationOutput (\s a -> s {_voServerValidationOutput = a})

instance FromJSON ValidationOutput where
  parseJSON =
    withObject
      "ValidationOutput"
      ( \x ->
          ValidationOutput'
            <$> (x .:? "status")
            <*> (x .:? "appValidationOutput")
            <*> (x .:? "latestValidationTime")
            <*> (x .:? "name")
            <*> (x .:? "statusMessage")
            <*> (x .:? "validationId")
            <*> (x .:? "serverValidationOutput")
      )

instance Hashable ValidationOutput

instance NFData ValidationOutput
