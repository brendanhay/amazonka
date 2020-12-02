{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Identity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Identity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about the type of identity that made the request.
--
--
--
-- /See:/ 'identity' smart constructor.
data Identity = Identity'
  { _iPrincipalId :: !(Maybe Text),
    _iType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Identity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iPrincipalId' - A unique identifier for the entity that made the call. For Time To Live, the principalId is "dynamodb.amazonaws.com".
--
-- * 'iType' - The type of the identity. For Time To Live, the type is "Service".
identity ::
  Identity
identity = Identity' {_iPrincipalId = Nothing, _iType = Nothing}

-- | A unique identifier for the entity that made the call. For Time To Live, the principalId is "dynamodb.amazonaws.com".
iPrincipalId :: Lens' Identity (Maybe Text)
iPrincipalId = lens _iPrincipalId (\s a -> s {_iPrincipalId = a})

-- | The type of the identity. For Time To Live, the type is "Service".
iType :: Lens' Identity (Maybe Text)
iType = lens _iType (\s a -> s {_iType = a})

instance FromJSON Identity where
  parseJSON =
    withObject
      "Identity"
      (\x -> Identity' <$> (x .:? "PrincipalId") <*> (x .:? "Type"))

instance Hashable Identity

instance NFData Identity
