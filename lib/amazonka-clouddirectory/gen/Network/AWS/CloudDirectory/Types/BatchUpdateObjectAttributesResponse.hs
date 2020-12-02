{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a @BatchUpdate@ response operation.
--
--
--
-- /See:/ 'batchUpdateObjectAttributesResponse' smart constructor.
newtype BatchUpdateObjectAttributesResponse = BatchUpdateObjectAttributesResponse'
  { _buoaObjectIdentifier ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchUpdateObjectAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'buoaObjectIdentifier' - ID that is associated with the object.
batchUpdateObjectAttributesResponse ::
  BatchUpdateObjectAttributesResponse
batchUpdateObjectAttributesResponse =
  BatchUpdateObjectAttributesResponse'
    { _buoaObjectIdentifier =
        Nothing
    }

-- | ID that is associated with the object.
buoaObjectIdentifier :: Lens' BatchUpdateObjectAttributesResponse (Maybe Text)
buoaObjectIdentifier = lens _buoaObjectIdentifier (\s a -> s {_buoaObjectIdentifier = a})

instance FromJSON BatchUpdateObjectAttributesResponse where
  parseJSON =
    withObject
      "BatchUpdateObjectAttributesResponse"
      ( \x ->
          BatchUpdateObjectAttributesResponse'
            <$> (x .:? "ObjectIdentifier")
      )

instance Hashable BatchUpdateObjectAttributesResponse

instance NFData BatchUpdateObjectAttributesResponse
