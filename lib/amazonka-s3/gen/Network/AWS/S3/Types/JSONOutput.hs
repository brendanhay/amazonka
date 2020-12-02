{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.JSONOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.JSONOutput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Specifies JSON as request's output serialization format.
--
--
--
-- /See:/ 'jsonOutput' smart constructor.
newtype JSONOutput = JSONOutput' {_joRecordDelimiter :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JSONOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'joRecordDelimiter' - The value used to separate individual records in the output. If no value is specified, Amazon S3 uses a newline character ('\n').
jsonOutput ::
  JSONOutput
jsonOutput = JSONOutput' {_joRecordDelimiter = Nothing}

-- | The value used to separate individual records in the output. If no value is specified, Amazon S3 uses a newline character ('\n').
joRecordDelimiter :: Lens' JSONOutput (Maybe Text)
joRecordDelimiter = lens _joRecordDelimiter (\s a -> s {_joRecordDelimiter = a})

instance Hashable JSONOutput

instance NFData JSONOutput

instance ToXML JSONOutput where
  toXML JSONOutput' {..} =
    mconcat ["RecordDelimiter" @= _joRecordDelimiter]
