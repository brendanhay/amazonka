{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ErrorDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ErrorDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about an error.
--
--
--
-- /See:/ 'errorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { _edErrorCode :: !(Maybe Text),
    _edErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ErrorDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edErrorCode' - The code associated with this error.
--
-- * 'edErrorMessage' - A message describing the error.
errorDetail ::
  ErrorDetail
errorDetail =
  ErrorDetail' {_edErrorCode = Nothing, _edErrorMessage = Nothing}

-- | The code associated with this error.
edErrorCode :: Lens' ErrorDetail (Maybe Text)
edErrorCode = lens _edErrorCode (\s a -> s {_edErrorCode = a})

-- | A message describing the error.
edErrorMessage :: Lens' ErrorDetail (Maybe Text)
edErrorMessage = lens _edErrorMessage (\s a -> s {_edErrorMessage = a})

instance FromJSON ErrorDetail where
  parseJSON =
    withObject
      "ErrorDetail"
      ( \x ->
          ErrorDetail' <$> (x .:? "ErrorCode") <*> (x .:? "ErrorMessage")
      )

instance Hashable ErrorDetail

instance NFData ErrorDetail
