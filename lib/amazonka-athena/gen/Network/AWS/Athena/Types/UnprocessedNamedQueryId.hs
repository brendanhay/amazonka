{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.UnprocessedNamedQueryId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.UnprocessedNamedQueryId where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a named query ID that could not be processed.
--
--
--
-- /See:/ 'unprocessedNamedQueryId' smart constructor.
data UnprocessedNamedQueryId = UnprocessedNamedQueryId'
  { _unqiNamedQueryId ::
      !(Maybe Text),
    _unqiErrorCode :: !(Maybe Text),
    _unqiErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnprocessedNamedQueryId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unqiNamedQueryId' - The unique identifier of the named query.
--
-- * 'unqiErrorCode' - The error code returned when the processing request for the named query failed, if applicable.
--
-- * 'unqiErrorMessage' - The error message returned when the processing request for the named query failed, if applicable.
unprocessedNamedQueryId ::
  UnprocessedNamedQueryId
unprocessedNamedQueryId =
  UnprocessedNamedQueryId'
    { _unqiNamedQueryId = Nothing,
      _unqiErrorCode = Nothing,
      _unqiErrorMessage = Nothing
    }

-- | The unique identifier of the named query.
unqiNamedQueryId :: Lens' UnprocessedNamedQueryId (Maybe Text)
unqiNamedQueryId = lens _unqiNamedQueryId (\s a -> s {_unqiNamedQueryId = a})

-- | The error code returned when the processing request for the named query failed, if applicable.
unqiErrorCode :: Lens' UnprocessedNamedQueryId (Maybe Text)
unqiErrorCode = lens _unqiErrorCode (\s a -> s {_unqiErrorCode = a})

-- | The error message returned when the processing request for the named query failed, if applicable.
unqiErrorMessage :: Lens' UnprocessedNamedQueryId (Maybe Text)
unqiErrorMessage = lens _unqiErrorMessage (\s a -> s {_unqiErrorMessage = a})

instance FromJSON UnprocessedNamedQueryId where
  parseJSON =
    withObject
      "UnprocessedNamedQueryId"
      ( \x ->
          UnprocessedNamedQueryId'
            <$> (x .:? "NamedQueryId")
            <*> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable UnprocessedNamedQueryId

instance NFData UnprocessedNamedQueryId
