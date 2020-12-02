{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ErrorDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ErrorDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'errorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { _edErrorType :: !(Maybe Text),
    _edErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ErrorDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edErrorType' - Undocumented member.
--
-- * 'edErrorMessage' - Undocumented member.
errorDetails ::
  ErrorDetails
errorDetails =
  ErrorDetails' {_edErrorType = Nothing, _edErrorMessage = Nothing}

-- | Undocumented member.
edErrorType :: Lens' ErrorDetails (Maybe Text)
edErrorType = lens _edErrorType (\s a -> s {_edErrorType = a})

-- | Undocumented member.
edErrorMessage :: Lens' ErrorDetails (Maybe Text)
edErrorMessage = lens _edErrorMessage (\s a -> s {_edErrorMessage = a})

instance FromJSON ErrorDetails where
  parseJSON =
    withObject
      "ErrorDetails"
      ( \x ->
          ErrorDetails' <$> (x .:? "ErrorType") <*> (x .:? "ErrorMessage")
      )

instance Hashable ErrorDetails

instance NFData ErrorDetails
