{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.UnprocessedIdentityId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.UnprocessedIdentityId where

import Network.AWS.CognitoIdentity.Types.CognitoErrorCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.
--
--
--
-- /See:/ 'unprocessedIdentityId' smart constructor.
data UnprocessedIdentityId = UnprocessedIdentityId'
  { _uiiErrorCode ::
      !(Maybe CognitoErrorCode),
    _uiiIdentityId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnprocessedIdentityId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiiErrorCode' - The error code indicating the type of error that occurred.
--
-- * 'uiiIdentityId' - A unique identifier in the format REGION:GUID.
unprocessedIdentityId ::
  UnprocessedIdentityId
unprocessedIdentityId =
  UnprocessedIdentityId'
    { _uiiErrorCode = Nothing,
      _uiiIdentityId = Nothing
    }

-- | The error code indicating the type of error that occurred.
uiiErrorCode :: Lens' UnprocessedIdentityId (Maybe CognitoErrorCode)
uiiErrorCode = lens _uiiErrorCode (\s a -> s {_uiiErrorCode = a})

-- | A unique identifier in the format REGION:GUID.
uiiIdentityId :: Lens' UnprocessedIdentityId (Maybe Text)
uiiIdentityId = lens _uiiIdentityId (\s a -> s {_uiiIdentityId = a})

instance FromJSON UnprocessedIdentityId where
  parseJSON =
    withObject
      "UnprocessedIdentityId"
      ( \x ->
          UnprocessedIdentityId'
            <$> (x .:? "ErrorCode") <*> (x .:? "IdentityId")
      )

instance Hashable UnprocessedIdentityId

instance NFData UnprocessedIdentityId
