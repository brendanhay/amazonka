{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ErrorDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ErrorDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the error.
--
-- /See:/ 'errorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { _edDetailedErrorCode ::
      !(Maybe Text),
    _edDetailedErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ErrorDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edDetailedErrorCode' - A detailed error code.
--
-- * 'edDetailedErrorMessage' - A detailed error message.
errorDetail ::
  ErrorDetail
errorDetail =
  ErrorDetail'
    { _edDetailedErrorCode = Nothing,
      _edDetailedErrorMessage = Nothing
    }

-- | A detailed error code.
edDetailedErrorCode :: Lens' ErrorDetail (Maybe Text)
edDetailedErrorCode = lens _edDetailedErrorCode (\s a -> s {_edDetailedErrorCode = a})

-- | A detailed error message.
edDetailedErrorMessage :: Lens' ErrorDetail (Maybe Text)
edDetailedErrorMessage = lens _edDetailedErrorMessage (\s a -> s {_edDetailedErrorMessage = a})

instance FromJSON ErrorDetail where
  parseJSON =
    withObject
      "ErrorDetail"
      ( \x ->
          ErrorDetail'
            <$> (x .:? "DetailedErrorCode") <*> (x .:? "DetailedErrorMessage")
      )

instance Hashable ErrorDetail

instance NFData ErrorDetail
