{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StackError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StackError where

import Network.AWS.AppStream.Types.StackErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a stack error.
--
--
--
-- /See:/ 'stackError' smart constructor.
data StackError = StackError'
  { _seErrorCode ::
      !(Maybe StackErrorCode),
    _seErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seErrorCode' - The error code.
--
-- * 'seErrorMessage' - The error message.
stackError ::
  StackError
stackError =
  StackError' {_seErrorCode = Nothing, _seErrorMessage = Nothing}

-- | The error code.
seErrorCode :: Lens' StackError (Maybe StackErrorCode)
seErrorCode = lens _seErrorCode (\s a -> s {_seErrorCode = a})

-- | The error message.
seErrorMessage :: Lens' StackError (Maybe Text)
seErrorMessage = lens _seErrorMessage (\s a -> s {_seErrorMessage = a})

instance FromJSON StackError where
  parseJSON =
    withObject
      "StackError"
      ( \x ->
          StackError' <$> (x .:? "ErrorCode") <*> (x .:? "ErrorMessage")
      )

instance Hashable StackError

instance NFData StackError
