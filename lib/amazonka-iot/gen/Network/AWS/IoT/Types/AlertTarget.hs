{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AlertTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AlertTarget where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure containing the alert target ARN and the role ARN.
--
--
--
-- /See:/ 'alertTarget' smart constructor.
data AlertTarget = AlertTarget'
  { _atAlertTargetARN :: !Text,
    _atRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AlertTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atAlertTargetARN' - The ARN of the notification target to which alerts are sent.
--
-- * 'atRoleARN' - The ARN of the role that grants permission to send alerts to the notification target.
alertTarget ::
  -- | 'atAlertTargetARN'
  Text ->
  -- | 'atRoleARN'
  Text ->
  AlertTarget
alertTarget pAlertTargetARN_ pRoleARN_ =
  AlertTarget'
    { _atAlertTargetARN = pAlertTargetARN_,
      _atRoleARN = pRoleARN_
    }

-- | The ARN of the notification target to which alerts are sent.
atAlertTargetARN :: Lens' AlertTarget Text
atAlertTargetARN = lens _atAlertTargetARN (\s a -> s {_atAlertTargetARN = a})

-- | The ARN of the role that grants permission to send alerts to the notification target.
atRoleARN :: Lens' AlertTarget Text
atRoleARN = lens _atRoleARN (\s a -> s {_atRoleARN = a})

instance FromJSON AlertTarget where
  parseJSON =
    withObject
      "AlertTarget"
      ( \x ->
          AlertTarget' <$> (x .: "alertTargetArn") <*> (x .: "roleArn")
      )

instance Hashable AlertTarget

instance NFData AlertTarget

instance ToJSON AlertTarget where
  toJSON AlertTarget' {..} =
    object
      ( catMaybes
          [ Just ("alertTargetArn" .= _atAlertTargetARN),
            Just ("roleArn" .= _atRoleARN)
          ]
      )
