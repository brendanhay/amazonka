{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'CreateObject' response operation.
--
--
--
-- /See:/ 'batchCreateObjectResponse' smart constructor.
newtype BatchCreateObjectResponse = BatchCreateObjectResponse'
  { _bcoObjectIdentifier ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchCreateObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcoObjectIdentifier' - The ID that is associated with the object.
batchCreateObjectResponse ::
  BatchCreateObjectResponse
batchCreateObjectResponse =
  BatchCreateObjectResponse' {_bcoObjectIdentifier = Nothing}

-- | The ID that is associated with the object.
bcoObjectIdentifier :: Lens' BatchCreateObjectResponse (Maybe Text)
bcoObjectIdentifier = lens _bcoObjectIdentifier (\s a -> s {_bcoObjectIdentifier = a})

instance FromJSON BatchCreateObjectResponse where
  parseJSON =
    withObject
      "BatchCreateObjectResponse"
      (\x -> BatchCreateObjectResponse' <$> (x .:? "ObjectIdentifier"))

instance Hashable BatchCreateObjectResponse

instance NFData BatchCreateObjectResponse
