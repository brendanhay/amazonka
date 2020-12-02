{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MongoDBSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MongoDBSettings where

import Network.AWS.DMS.Types.AuthMechanismValue
import Network.AWS.DMS.Types.AuthTypeValue
import Network.AWS.DMS.Types.NestingLevelValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines a MongoDB endpoint.
--
--
--
-- /See:/ 'mongoDBSettings' smart constructor.
data MongoDBSettings = MongoDBSettings'
  { _mdsServerName ::
      !(Maybe Text),
    _mdsAuthMechanism :: !(Maybe AuthMechanismValue),
    _mdsUsername :: !(Maybe Text),
    _mdsKMSKeyId :: !(Maybe Text),
    _mdsPassword :: !(Maybe (Sensitive Text)),
    _mdsNestingLevel :: !(Maybe NestingLevelValue),
    _mdsDatabaseName :: !(Maybe Text),
    _mdsDocsToInvestigate :: !(Maybe Text),
    _mdsAuthSource :: !(Maybe Text),
    _mdsExtractDocId :: !(Maybe Text),
    _mdsAuthType :: !(Maybe AuthTypeValue),
    _mdsPort :: !(Maybe Int)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'MongoDBSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdsServerName' - The name of the server on the MongoDB source endpoint.
--
-- * 'mdsAuthMechanism' - The authentication mechanism you use to access the MongoDB source endpoint. For the default value, in MongoDB version 2.x, @"default"@ is @"mongodb_cr"@ . For MongoDB version 3.x or later, @"default"@ is @"scram_sha_1"@ . This setting isn't used when @AuthType@ is set to @"no"@ .
--
-- * 'mdsUsername' - The user name you use to access the MongoDB source endpoint.
--
-- * 'mdsKMSKeyId' - The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'mdsPassword' - The password for the user account you use to access the MongoDB source endpoint.
--
-- * 'mdsNestingLevel' - Specifies either document or table mode.  Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
--
-- * 'mdsDatabaseName' - The database name on the MongoDB source endpoint.
--
-- * 'mdsDocsToInvestigate' - Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ .  Must be a positive value greater than @0@ . Default value is @1000@ .
--
-- * 'mdsAuthSource' - The MongoDB database name. This setting isn't used when @AuthType@ is set to @"no"@ .  The default is @"admin"@ .
--
-- * 'mdsExtractDocId' - Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ .  Default value is @"false"@ .
--
-- * 'mdsAuthType' - The authentication type you use to access the MongoDB source endpoint. When when set to @"no"@ , user name and password parameters are not used and can be empty.
--
-- * 'mdsPort' - The port value for the MongoDB source endpoint.
mongoDBSettings ::
  MongoDBSettings
mongoDBSettings =
  MongoDBSettings'
    { _mdsServerName = Nothing,
      _mdsAuthMechanism = Nothing,
      _mdsUsername = Nothing,
      _mdsKMSKeyId = Nothing,
      _mdsPassword = Nothing,
      _mdsNestingLevel = Nothing,
      _mdsDatabaseName = Nothing,
      _mdsDocsToInvestigate = Nothing,
      _mdsAuthSource = Nothing,
      _mdsExtractDocId = Nothing,
      _mdsAuthType = Nothing,
      _mdsPort = Nothing
    }

-- | The name of the server on the MongoDB source endpoint.
mdsServerName :: Lens' MongoDBSettings (Maybe Text)
mdsServerName = lens _mdsServerName (\s a -> s {_mdsServerName = a})

-- | The authentication mechanism you use to access the MongoDB source endpoint. For the default value, in MongoDB version 2.x, @"default"@ is @"mongodb_cr"@ . For MongoDB version 3.x or later, @"default"@ is @"scram_sha_1"@ . This setting isn't used when @AuthType@ is set to @"no"@ .
mdsAuthMechanism :: Lens' MongoDBSettings (Maybe AuthMechanismValue)
mdsAuthMechanism = lens _mdsAuthMechanism (\s a -> s {_mdsAuthMechanism = a})

-- | The user name you use to access the MongoDB source endpoint.
mdsUsername :: Lens' MongoDBSettings (Maybe Text)
mdsUsername = lens _mdsUsername (\s a -> s {_mdsUsername = a})

-- | The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
mdsKMSKeyId :: Lens' MongoDBSettings (Maybe Text)
mdsKMSKeyId = lens _mdsKMSKeyId (\s a -> s {_mdsKMSKeyId = a})

-- | The password for the user account you use to access the MongoDB source endpoint.
mdsPassword :: Lens' MongoDBSettings (Maybe Text)
mdsPassword = lens _mdsPassword (\s a -> s {_mdsPassword = a}) . mapping _Sensitive

-- | Specifies either document or table mode.  Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
mdsNestingLevel :: Lens' MongoDBSettings (Maybe NestingLevelValue)
mdsNestingLevel = lens _mdsNestingLevel (\s a -> s {_mdsNestingLevel = a})

-- | The database name on the MongoDB source endpoint.
mdsDatabaseName :: Lens' MongoDBSettings (Maybe Text)
mdsDatabaseName = lens _mdsDatabaseName (\s a -> s {_mdsDatabaseName = a})

-- | Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ .  Must be a positive value greater than @0@ . Default value is @1000@ .
mdsDocsToInvestigate :: Lens' MongoDBSettings (Maybe Text)
mdsDocsToInvestigate = lens _mdsDocsToInvestigate (\s a -> s {_mdsDocsToInvestigate = a})

-- | The MongoDB database name. This setting isn't used when @AuthType@ is set to @"no"@ .  The default is @"admin"@ .
mdsAuthSource :: Lens' MongoDBSettings (Maybe Text)
mdsAuthSource = lens _mdsAuthSource (\s a -> s {_mdsAuthSource = a})

-- | Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ .  Default value is @"false"@ .
mdsExtractDocId :: Lens' MongoDBSettings (Maybe Text)
mdsExtractDocId = lens _mdsExtractDocId (\s a -> s {_mdsExtractDocId = a})

-- | The authentication type you use to access the MongoDB source endpoint. When when set to @"no"@ , user name and password parameters are not used and can be empty.
mdsAuthType :: Lens' MongoDBSettings (Maybe AuthTypeValue)
mdsAuthType = lens _mdsAuthType (\s a -> s {_mdsAuthType = a})

-- | The port value for the MongoDB source endpoint.
mdsPort :: Lens' MongoDBSettings (Maybe Int)
mdsPort = lens _mdsPort (\s a -> s {_mdsPort = a})

instance FromJSON MongoDBSettings where
  parseJSON =
    withObject
      "MongoDBSettings"
      ( \x ->
          MongoDBSettings'
            <$> (x .:? "ServerName")
            <*> (x .:? "AuthMechanism")
            <*> (x .:? "Username")
            <*> (x .:? "KmsKeyId")
            <*> (x .:? "Password")
            <*> (x .:? "NestingLevel")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "DocsToInvestigate")
            <*> (x .:? "AuthSource")
            <*> (x .:? "ExtractDocId")
            <*> (x .:? "AuthType")
            <*> (x .:? "Port")
      )

instance Hashable MongoDBSettings

instance NFData MongoDBSettings

instance ToJSON MongoDBSettings where
  toJSON MongoDBSettings' {..} =
    object
      ( catMaybes
          [ ("ServerName" .=) <$> _mdsServerName,
            ("AuthMechanism" .=) <$> _mdsAuthMechanism,
            ("Username" .=) <$> _mdsUsername,
            ("KmsKeyId" .=) <$> _mdsKMSKeyId,
            ("Password" .=) <$> _mdsPassword,
            ("NestingLevel" .=) <$> _mdsNestingLevel,
            ("DatabaseName" .=) <$> _mdsDatabaseName,
            ("DocsToInvestigate" .=) <$> _mdsDocsToInvestigate,
            ("AuthSource" .=) <$> _mdsAuthSource,
            ("ExtractDocId" .=) <$> _mdsExtractDocId,
            ("AuthType" .=) <$> _mdsAuthType,
            ("Port" .=) <$> _mdsPort
          ]
      )
