{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceStateChangeReason where

import Network.AWS.EMR.Types.InstanceStateChangeReasonCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of the status change reason for the instance.
--
--
--
-- /See:/ 'instanceStateChangeReason' smart constructor.
data InstanceStateChangeReason = InstanceStateChangeReason'
  { _iscrCode ::
      !(Maybe InstanceStateChangeReasonCode),
    _iscrMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscrCode' - The programmable code for the state change reason.
--
-- * 'iscrMessage' - The status change reason description.
instanceStateChangeReason ::
  InstanceStateChangeReason
instanceStateChangeReason =
  InstanceStateChangeReason'
    { _iscrCode = Nothing,
      _iscrMessage = Nothing
    }

-- | The programmable code for the state change reason.
iscrCode :: Lens' InstanceStateChangeReason (Maybe InstanceStateChangeReasonCode)
iscrCode = lens _iscrCode (\s a -> s {_iscrCode = a})

-- | The status change reason description.
iscrMessage :: Lens' InstanceStateChangeReason (Maybe Text)
iscrMessage = lens _iscrMessage (\s a -> s {_iscrMessage = a})

instance FromJSON InstanceStateChangeReason where
  parseJSON =
    withObject
      "InstanceStateChangeReason"
      ( \x ->
          InstanceStateChangeReason'
            <$> (x .:? "Code") <*> (x .:? "Message")
      )

instance Hashable InstanceStateChangeReason

instance NFData InstanceStateChangeReason
