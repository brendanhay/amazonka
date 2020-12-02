{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileSummary where

import Network.AWS.CloudFront.Types.EncryptionEntities
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The field-level encryption profile summary.
--
--
--
-- /See:/ 'fieldLevelEncryptionProfileSummary' smart constructor.
data FieldLevelEncryptionProfileSummary = FieldLevelEncryptionProfileSummary'
  { _flepsComment ::
      !(Maybe Text),
    _flepsId :: !Text,
    _flepsLastModifiedTime ::
      !ISO8601,
    _flepsName :: !Text,
    _flepsEncryptionEntities ::
      !EncryptionEntities
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FieldLevelEncryptionProfileSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flepsComment' - An optional comment for the field-level encryption profile summary.
--
-- * 'flepsId' - ID for the field-level encryption profile summary.
--
-- * 'flepsLastModifiedTime' - The time when the the field-level encryption profile summary was last updated.
--
-- * 'flepsName' - Name for the field-level encryption profile summary.
--
-- * 'flepsEncryptionEntities' - A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
fieldLevelEncryptionProfileSummary ::
  -- | 'flepsId'
  Text ->
  -- | 'flepsLastModifiedTime'
  UTCTime ->
  -- | 'flepsName'
  Text ->
  -- | 'flepsEncryptionEntities'
  EncryptionEntities ->
  FieldLevelEncryptionProfileSummary
fieldLevelEncryptionProfileSummary
  pId_
  pLastModifiedTime_
  pName_
  pEncryptionEntities_ =
    FieldLevelEncryptionProfileSummary'
      { _flepsComment = Nothing,
        _flepsId = pId_,
        _flepsLastModifiedTime = _Time # pLastModifiedTime_,
        _flepsName = pName_,
        _flepsEncryptionEntities = pEncryptionEntities_
      }

-- | An optional comment for the field-level encryption profile summary.
flepsComment :: Lens' FieldLevelEncryptionProfileSummary (Maybe Text)
flepsComment = lens _flepsComment (\s a -> s {_flepsComment = a})

-- | ID for the field-level encryption profile summary.
flepsId :: Lens' FieldLevelEncryptionProfileSummary Text
flepsId = lens _flepsId (\s a -> s {_flepsId = a})

-- | The time when the the field-level encryption profile summary was last updated.
flepsLastModifiedTime :: Lens' FieldLevelEncryptionProfileSummary UTCTime
flepsLastModifiedTime = lens _flepsLastModifiedTime (\s a -> s {_flepsLastModifiedTime = a}) . _Time

-- | Name for the field-level encryption profile summary.
flepsName :: Lens' FieldLevelEncryptionProfileSummary Text
flepsName = lens _flepsName (\s a -> s {_flepsName = a})

-- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
flepsEncryptionEntities :: Lens' FieldLevelEncryptionProfileSummary EncryptionEntities
flepsEncryptionEntities = lens _flepsEncryptionEntities (\s a -> s {_flepsEncryptionEntities = a})

instance FromXML FieldLevelEncryptionProfileSummary where
  parseXML x =
    FieldLevelEncryptionProfileSummary'
      <$> (x .@? "Comment")
      <*> (x .@ "Id")
      <*> (x .@ "LastModifiedTime")
      <*> (x .@ "Name")
      <*> (x .@ "EncryptionEntities")

instance Hashable FieldLevelEncryptionProfileSummary

instance NFData FieldLevelEncryptionProfileSummary
