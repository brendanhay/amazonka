{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'AttachToIndex' response operation.
--
--
--
-- /See:/ 'batchAttachToIndexResponse' smart constructor.
newtype BatchAttachToIndexResponse = BatchAttachToIndexResponse'
  { _batiAttachedObjectIdentifier ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchAttachToIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'batiAttachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was attached to the index.
batchAttachToIndexResponse ::
  BatchAttachToIndexResponse
batchAttachToIndexResponse =
  BatchAttachToIndexResponse'
    { _batiAttachedObjectIdentifier =
        Nothing
    }

-- | The @ObjectIdentifier@ of the object that was attached to the index.
batiAttachedObjectIdentifier :: Lens' BatchAttachToIndexResponse (Maybe Text)
batiAttachedObjectIdentifier = lens _batiAttachedObjectIdentifier (\s a -> s {_batiAttachedObjectIdentifier = a})

instance FromJSON BatchAttachToIndexResponse where
  parseJSON =
    withObject
      "BatchAttachToIndexResponse"
      ( \x ->
          BatchAttachToIndexResponse' <$> (x .:? "AttachedObjectIdentifier")
      )

instance Hashable BatchAttachToIndexResponse

instance NFData BatchAttachToIndexResponse
