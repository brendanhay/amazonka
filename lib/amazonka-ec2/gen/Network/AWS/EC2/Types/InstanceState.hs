{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceState where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceStateName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the current state of an instance.
--
--
--
-- /See:/ 'instanceState' smart constructor.
data InstanceState = InstanceState'
  { _isName :: !InstanceStateName,
    _isCode :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isName' - The current state of the instance.
--
-- * 'isCode' - The state of the instance as a 16-bit unsigned integer.  The high byte is all of the bits between 2^8 and (2^16)-1, which equals decimal values between 256 and 65,535. These numerical values are used for internal purposes and should be ignored. The low byte is all of the bits between 2^0 and (2^8)-1, which equals decimal values between 0 and 255.  The valid values for instance-state-code will all be in the range of the low byte and they are:     * @0@ : @pending@      * @16@ : @running@      * @32@ : @shutting-down@      * @48@ : @terminated@      * @64@ : @stopping@      * @80@ : @stopped@  You can ignore the high byte value by zeroing out all of the bits above 2^8 or 256 in decimal.
instanceState ::
  -- | 'isName'
  InstanceStateName ->
  -- | 'isCode'
  Int ->
  InstanceState
instanceState pName_ pCode_ =
  InstanceState' {_isName = pName_, _isCode = pCode_}

-- | The current state of the instance.
isName :: Lens' InstanceState InstanceStateName
isName = lens _isName (\s a -> s {_isName = a})

-- | The state of the instance as a 16-bit unsigned integer.  The high byte is all of the bits between 2^8 and (2^16)-1, which equals decimal values between 256 and 65,535. These numerical values are used for internal purposes and should be ignored. The low byte is all of the bits between 2^0 and (2^8)-1, which equals decimal values between 0 and 255.  The valid values for instance-state-code will all be in the range of the low byte and they are:     * @0@ : @pending@      * @16@ : @running@      * @32@ : @shutting-down@      * @48@ : @terminated@      * @64@ : @stopping@      * @80@ : @stopped@  You can ignore the high byte value by zeroing out all of the bits above 2^8 or 256 in decimal.
isCode :: Lens' InstanceState Int
isCode = lens _isCode (\s a -> s {_isCode = a})

instance FromXML InstanceState where
  parseXML x = InstanceState' <$> (x .@ "name") <*> (x .@ "code")

instance Hashable InstanceState

instance NFData InstanceState
