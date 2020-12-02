{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ScanRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ScanRange where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Specifies the byte range of the object to get the records from. A record is processed when its first byte is contained by the range. This parameter is optional, but when specified, it must not be empty. See RFC 2616, Section 14.35.1 about how to specify the start and end of the range.
--
--
--
-- /See:/ 'scanRange' smart constructor.
data ScanRange = ScanRange'
  { _srStart :: !(Maybe Integer),
    _srEnd :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScanRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srStart' - Specifies the start of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is 0. If only start is supplied, it means scan from that point to the end of the file.For example; @<scanrange><start>50</start></scanrange>@ means scan from byte 50 until the end of the file.
--
-- * 'srEnd' - Specifies the end of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is one less than the size of the object being queried. If only the End parameter is supplied, it is interpreted to mean scan the last N bytes of the file. For example, @<scanrange><end>50</end></scanrange>@ means scan the last 50 bytes.
scanRange ::
  ScanRange
scanRange = ScanRange' {_srStart = Nothing, _srEnd = Nothing}

-- | Specifies the start of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is 0. If only start is supplied, it means scan from that point to the end of the file.For example; @<scanrange><start>50</start></scanrange>@ means scan from byte 50 until the end of the file.
srStart :: Lens' ScanRange (Maybe Integer)
srStart = lens _srStart (\s a -> s {_srStart = a})

-- | Specifies the end of the byte range. This parameter is optional. Valid values: non-negative integers. The default value is one less than the size of the object being queried. If only the End parameter is supplied, it is interpreted to mean scan the last N bytes of the file. For example, @<scanrange><end>50</end></scanrange>@ means scan the last 50 bytes.
srEnd :: Lens' ScanRange (Maybe Integer)
srEnd = lens _srEnd (\s a -> s {_srEnd = a})

instance Hashable ScanRange

instance NFData ScanRange

instance ToXML ScanRange where
  toXML ScanRange' {..} =
    mconcat ["Start" @= _srStart, "End" @= _srEnd]
