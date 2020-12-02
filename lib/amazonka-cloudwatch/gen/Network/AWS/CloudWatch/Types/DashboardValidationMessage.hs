{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.DashboardValidationMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.DashboardValidationMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An error or warning for the operation.
--
--
--
-- /See:/ 'dashboardValidationMessage' smart constructor.
data DashboardValidationMessage = DashboardValidationMessage'
  { _dvmDataPath ::
      !(Maybe Text),
    _dvmMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DashboardValidationMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvmDataPath' - The data path related to the message.
--
-- * 'dvmMessage' - A message describing the error or warning.
dashboardValidationMessage ::
  DashboardValidationMessage
dashboardValidationMessage =
  DashboardValidationMessage'
    { _dvmDataPath = Nothing,
      _dvmMessage = Nothing
    }

-- | The data path related to the message.
dvmDataPath :: Lens' DashboardValidationMessage (Maybe Text)
dvmDataPath = lens _dvmDataPath (\s a -> s {_dvmDataPath = a})

-- | A message describing the error or warning.
dvmMessage :: Lens' DashboardValidationMessage (Maybe Text)
dvmMessage = lens _dvmMessage (\s a -> s {_dvmMessage = a})

instance FromXML DashboardValidationMessage where
  parseXML x =
    DashboardValidationMessage'
      <$> (x .@? "DataPath") <*> (x .@? "Message")

instance Hashable DashboardValidationMessage

instance NFData DashboardValidationMessage
