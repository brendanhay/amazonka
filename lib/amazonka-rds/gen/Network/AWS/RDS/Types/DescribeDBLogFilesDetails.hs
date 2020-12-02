{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DescribeDBLogFilesDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DescribeDBLogFilesDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used as a response element to @DescribeDBLogFiles@ .
--
--
--
-- /See:/ 'describeDBLogFilesDetails' smart constructor.
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails'
  { _ddlfdLastWritten ::
      !(Maybe Integer),
    _ddlfdSize :: !(Maybe Integer),
    _ddlfdLogFileName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBLogFilesDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddlfdLastWritten' - A POSIX timestamp when the last log entry was written.
--
-- * 'ddlfdSize' - The size, in bytes, of the log file for the specified DB instance.
--
-- * 'ddlfdLogFileName' - The name of the log file for the specified DB instance.
describeDBLogFilesDetails ::
  DescribeDBLogFilesDetails
describeDBLogFilesDetails =
  DescribeDBLogFilesDetails'
    { _ddlfdLastWritten = Nothing,
      _ddlfdSize = Nothing,
      _ddlfdLogFileName = Nothing
    }

-- | A POSIX timestamp when the last log entry was written.
ddlfdLastWritten :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddlfdLastWritten = lens _ddlfdLastWritten (\s a -> s {_ddlfdLastWritten = a})

-- | The size, in bytes, of the log file for the specified DB instance.
ddlfdSize :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddlfdSize = lens _ddlfdSize (\s a -> s {_ddlfdSize = a})

-- | The name of the log file for the specified DB instance.
ddlfdLogFileName :: Lens' DescribeDBLogFilesDetails (Maybe Text)
ddlfdLogFileName = lens _ddlfdLogFileName (\s a -> s {_ddlfdLogFileName = a})

instance FromXML DescribeDBLogFilesDetails where
  parseXML x =
    DescribeDBLogFilesDetails'
      <$> (x .@? "LastWritten") <*> (x .@? "Size") <*> (x .@? "LogFileName")

instance Hashable DescribeDBLogFilesDetails

instance NFData DescribeDBLogFilesDetails
