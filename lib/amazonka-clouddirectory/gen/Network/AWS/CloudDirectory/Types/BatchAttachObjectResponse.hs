{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output batch 'AttachObject' response operation.
--
--
--
-- /See:/ 'batchAttachObjectResponse' smart constructor.
newtype BatchAttachObjectResponse = BatchAttachObjectResponse'
  { _baoAttachedObjectIdentifier ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchAttachObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baoAttachedObjectIdentifier' - The @ObjectIdentifier@ of the object that has been attached.
batchAttachObjectResponse ::
  BatchAttachObjectResponse
batchAttachObjectResponse =
  BatchAttachObjectResponse'
    { _baoAttachedObjectIdentifier =
        Nothing
    }

-- | The @ObjectIdentifier@ of the object that has been attached.
baoAttachedObjectIdentifier :: Lens' BatchAttachObjectResponse (Maybe Text)
baoAttachedObjectIdentifier = lens _baoAttachedObjectIdentifier (\s a -> s {_baoAttachedObjectIdentifier = a})

instance FromJSON BatchAttachObjectResponse where
  parseJSON =
    withObject
      "BatchAttachObjectResponse"
      ( \x ->
          BatchAttachObjectResponse' <$> (x .:? "attachedObjectIdentifier")
      )

instance Hashable BatchAttachObjectResponse

instance NFData BatchAttachObjectResponse
