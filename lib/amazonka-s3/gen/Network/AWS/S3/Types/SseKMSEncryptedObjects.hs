{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SseKMSEncryptedObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SseKMSEncryptedObjects where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.SseKMSEncryptedObjectsStatus

-- | A container for filter information for the selection of S3 objects encrypted with AWS KMS.
--
--
--
-- /See:/ 'sseKMSEncryptedObjects' smart constructor.
newtype SseKMSEncryptedObjects = SseKMSEncryptedObjects'
  { _skeoStatus ::
      SseKMSEncryptedObjectsStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SseKMSEncryptedObjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skeoStatus' - Specifies whether Amazon S3 replicates objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service.
sseKMSEncryptedObjects ::
  -- | 'skeoStatus'
  SseKMSEncryptedObjectsStatus ->
  SseKMSEncryptedObjects
sseKMSEncryptedObjects pStatus_ =
  SseKMSEncryptedObjects' {_skeoStatus = pStatus_}

-- | Specifies whether Amazon S3 replicates objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service.
skeoStatus :: Lens' SseKMSEncryptedObjects SseKMSEncryptedObjectsStatus
skeoStatus = lens _skeoStatus (\s a -> s {_skeoStatus = a})

instance FromXML SseKMSEncryptedObjects where
  parseXML x = SseKMSEncryptedObjects' <$> (x .@ "Status")

instance Hashable SseKMSEncryptedObjects

instance NFData SseKMSEncryptedObjects

instance ToXML SseKMSEncryptedObjects where
  toXML SseKMSEncryptedObjects' {..} =
    mconcat ["Status" @= _skeoStatus]
