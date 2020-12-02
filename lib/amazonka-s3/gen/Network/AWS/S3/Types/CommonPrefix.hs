{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CommonPrefix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CommonPrefix where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Container for all (if there are any) keys between Prefix and the next occurrence of the string specified by a delimiter. CommonPrefixes lists keys that act like subdirectories in the directory specified by Prefix. For example, if the prefix is notes/ and the delimiter is a slash (/) as in notes/summer/july, the common prefix is notes/summer/.
--
--
--
-- /See:/ 'commonPrefix' smart constructor.
newtype CommonPrefix = CommonPrefix' {_cpPrefix :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CommonPrefix' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPrefix' - Container for the specified common prefix.
commonPrefix ::
  CommonPrefix
commonPrefix = CommonPrefix' {_cpPrefix = Nothing}

-- | Container for the specified common prefix.
cpPrefix :: Lens' CommonPrefix (Maybe Text)
cpPrefix = lens _cpPrefix (\s a -> s {_cpPrefix = a})

instance FromXML CommonPrefix where
  parseXML x = CommonPrefix' <$> (x .@? "Prefix")

instance Hashable CommonPrefix

instance NFData CommonPrefix
