{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationSummary where

import Network.AWS.KinesisAnalytics.Types.ApplicationStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides application summary information, including the application Amazon Resource Name (ARN), name, and status.
--
--
--
-- /See:/ 'applicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { _asApplicationName ::
      !Text,
    _asApplicationARN :: !Text,
    _asApplicationStatus :: !ApplicationStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asApplicationName' - Name of the application.
--
-- * 'asApplicationARN' - ARN of the application.
--
-- * 'asApplicationStatus' - Status of the application.
applicationSummary ::
  -- | 'asApplicationName'
  Text ->
  -- | 'asApplicationARN'
  Text ->
  -- | 'asApplicationStatus'
  ApplicationStatus ->
  ApplicationSummary
applicationSummary
  pApplicationName_
  pApplicationARN_
  pApplicationStatus_ =
    ApplicationSummary'
      { _asApplicationName = pApplicationName_,
        _asApplicationARN = pApplicationARN_,
        _asApplicationStatus = pApplicationStatus_
      }

-- | Name of the application.
asApplicationName :: Lens' ApplicationSummary Text
asApplicationName = lens _asApplicationName (\s a -> s {_asApplicationName = a})

-- | ARN of the application.
asApplicationARN :: Lens' ApplicationSummary Text
asApplicationARN = lens _asApplicationARN (\s a -> s {_asApplicationARN = a})

-- | Status of the application.
asApplicationStatus :: Lens' ApplicationSummary ApplicationStatus
asApplicationStatus = lens _asApplicationStatus (\s a -> s {_asApplicationStatus = a})

instance FromJSON ApplicationSummary where
  parseJSON =
    withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            <$> (x .: "ApplicationName")
            <*> (x .: "ApplicationARN")
            <*> (x .: "ApplicationStatus")
      )

instance Hashable ApplicationSummary

instance NFData ApplicationSummary
