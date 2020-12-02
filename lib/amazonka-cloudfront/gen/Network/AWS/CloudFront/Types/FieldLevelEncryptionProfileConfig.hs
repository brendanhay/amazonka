{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig where

import Network.AWS.CloudFront.Types.EncryptionEntities
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex data type of profiles for the field-level encryption.
--
--
--
-- /See:/ 'fieldLevelEncryptionProfileConfig' smart constructor.
data FieldLevelEncryptionProfileConfig = FieldLevelEncryptionProfileConfig'
  { _flepcComment ::
      !(Maybe Text),
    _flepcName :: !Text,
    _flepcCallerReference ::
      !Text,
    _flepcEncryptionEntities ::
      !EncryptionEntities
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FieldLevelEncryptionProfileConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flepcComment' - An optional comment for the field-level encryption profile.
--
-- * 'flepcName' - Profile name for the field-level encryption profile.
--
-- * 'flepcCallerReference' - A unique number that ensures that the request can't be replayed.
--
-- * 'flepcEncryptionEntities' - A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
fieldLevelEncryptionProfileConfig ::
  -- | 'flepcName'
  Text ->
  -- | 'flepcCallerReference'
  Text ->
  -- | 'flepcEncryptionEntities'
  EncryptionEntities ->
  FieldLevelEncryptionProfileConfig
fieldLevelEncryptionProfileConfig
  pName_
  pCallerReference_
  pEncryptionEntities_ =
    FieldLevelEncryptionProfileConfig'
      { _flepcComment = Nothing,
        _flepcName = pName_,
        _flepcCallerReference = pCallerReference_,
        _flepcEncryptionEntities = pEncryptionEntities_
      }

-- | An optional comment for the field-level encryption profile.
flepcComment :: Lens' FieldLevelEncryptionProfileConfig (Maybe Text)
flepcComment = lens _flepcComment (\s a -> s {_flepcComment = a})

-- | Profile name for the field-level encryption profile.
flepcName :: Lens' FieldLevelEncryptionProfileConfig Text
flepcName = lens _flepcName (\s a -> s {_flepcName = a})

-- | A unique number that ensures that the request can't be replayed.
flepcCallerReference :: Lens' FieldLevelEncryptionProfileConfig Text
flepcCallerReference = lens _flepcCallerReference (\s a -> s {_flepcCallerReference = a})

-- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
flepcEncryptionEntities :: Lens' FieldLevelEncryptionProfileConfig EncryptionEntities
flepcEncryptionEntities = lens _flepcEncryptionEntities (\s a -> s {_flepcEncryptionEntities = a})

instance FromXML FieldLevelEncryptionProfileConfig where
  parseXML x =
    FieldLevelEncryptionProfileConfig'
      <$> (x .@? "Comment")
      <*> (x .@ "Name")
      <*> (x .@ "CallerReference")
      <*> (x .@ "EncryptionEntities")

instance Hashable FieldLevelEncryptionProfileConfig

instance NFData FieldLevelEncryptionProfileConfig

instance ToXML FieldLevelEncryptionProfileConfig where
  toXML FieldLevelEncryptionProfileConfig' {..} =
    mconcat
      [ "Comment" @= _flepcComment,
        "Name" @= _flepcName,
        "CallerReference" @= _flepcCallerReference,
        "EncryptionEntities" @= _flepcEncryptionEntities
      ]
