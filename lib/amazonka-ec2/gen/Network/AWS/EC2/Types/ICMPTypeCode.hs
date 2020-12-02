{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ICMPTypeCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ICMPTypeCode where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the ICMP type and code.
--
--
--
-- /See:/ 'icmpTypeCode' smart constructor.
data ICMPTypeCode = ICMPTypeCode'
  { _itcCode :: !(Maybe Int),
    _itcType :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ICMPTypeCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itcCode' - The ICMP code. A value of -1 means all codes for the specified ICMP type.
--
-- * 'itcType' - The ICMP type. A value of -1 means all types.
icmpTypeCode ::
  ICMPTypeCode
icmpTypeCode =
  ICMPTypeCode' {_itcCode = Nothing, _itcType = Nothing}

-- | The ICMP code. A value of -1 means all codes for the specified ICMP type.
itcCode :: Lens' ICMPTypeCode (Maybe Int)
itcCode = lens _itcCode (\s a -> s {_itcCode = a})

-- | The ICMP type. A value of -1 means all types.
itcType :: Lens' ICMPTypeCode (Maybe Int)
itcType = lens _itcType (\s a -> s {_itcType = a})

instance FromXML ICMPTypeCode where
  parseXML x = ICMPTypeCode' <$> (x .@? "code") <*> (x .@? "type")

instance Hashable ICMPTypeCode

instance NFData ICMPTypeCode

instance ToQuery ICMPTypeCode where
  toQuery ICMPTypeCode' {..} =
    mconcat ["Code" =: _itcCode, "Type" =: _itcType]
