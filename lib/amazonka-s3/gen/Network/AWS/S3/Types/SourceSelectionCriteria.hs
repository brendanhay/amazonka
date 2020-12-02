{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SourceSelectionCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SourceSelectionCriteria where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.SseKMSEncryptedObjects

-- | A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
--
--
--
-- /See:/ 'sourceSelectionCriteria' smart constructor.
newtype SourceSelectionCriteria = SourceSelectionCriteria'
  { _sscSseKMSEncryptedObjects ::
      Maybe SseKMSEncryptedObjects
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceSelectionCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscSseKMSEncryptedObjects' - A container for filter information for the selection of Amazon S3 objects encrypted with AWS KMS. If you include @SourceSelectionCriteria@ in the replication configuration, this element is required.
sourceSelectionCriteria ::
  SourceSelectionCriteria
sourceSelectionCriteria =
  SourceSelectionCriteria' {_sscSseKMSEncryptedObjects = Nothing}

-- | A container for filter information for the selection of Amazon S3 objects encrypted with AWS KMS. If you include @SourceSelectionCriteria@ in the replication configuration, this element is required.
sscSseKMSEncryptedObjects :: Lens' SourceSelectionCriteria (Maybe SseKMSEncryptedObjects)
sscSseKMSEncryptedObjects = lens _sscSseKMSEncryptedObjects (\s a -> s {_sscSseKMSEncryptedObjects = a})

instance FromXML SourceSelectionCriteria where
  parseXML x =
    SourceSelectionCriteria' <$> (x .@? "SseKmsEncryptedObjects")

instance Hashable SourceSelectionCriteria

instance NFData SourceSelectionCriteria

instance ToXML SourceSelectionCriteria where
  toXML SourceSelectionCriteria' {..} =
    mconcat ["SseKmsEncryptedObjects" @= _sscSseKMSEncryptedObjects]
