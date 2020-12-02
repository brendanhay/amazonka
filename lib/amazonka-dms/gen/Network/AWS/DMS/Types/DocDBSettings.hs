{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DocDBSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DocDBSettings where

import Network.AWS.DMS.Types.NestingLevelValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines a DocumentDB endpoint.
--
--
--
-- /See:/ 'docDBSettings' smart constructor.
data DocDBSettings = DocDBSettings'
  { _ddsServerName ::
      !(Maybe Text),
    _ddsUsername :: !(Maybe Text),
    _ddsKMSKeyId :: !(Maybe Text),
    _ddsPassword :: !(Maybe (Sensitive Text)),
    _ddsNestingLevel :: !(Maybe NestingLevelValue),
    _ddsDatabaseName :: !(Maybe Text),
    _ddsDocsToInvestigate :: !(Maybe Int),
    _ddsExtractDocId :: !(Maybe Bool),
    _ddsPort :: !(Maybe Int)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocDBSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsServerName' - The name of the server on the DocumentDB source endpoint.
--
-- * 'ddsUsername' - The user name you use to access the DocumentDB source endpoint.
--
-- * 'ddsKMSKeyId' - The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'ddsPassword' - The password for the user account you use to access the DocumentDB source endpoint.
--
-- * 'ddsNestingLevel' - Specifies either document or table mode.  Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
--
-- * 'ddsDatabaseName' - The database name on the DocumentDB source endpoint.
--
-- * 'ddsDocsToInvestigate' - Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ .  Must be a positive value greater than @0@ . Default value is @1000@ .
--
-- * 'ddsExtractDocId' - Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ .  Default value is @"false"@ .
--
-- * 'ddsPort' - The port value for the DocumentDB source endpoint.
docDBSettings ::
  DocDBSettings
docDBSettings =
  DocDBSettings'
    { _ddsServerName = Nothing,
      _ddsUsername = Nothing,
      _ddsKMSKeyId = Nothing,
      _ddsPassword = Nothing,
      _ddsNestingLevel = Nothing,
      _ddsDatabaseName = Nothing,
      _ddsDocsToInvestigate = Nothing,
      _ddsExtractDocId = Nothing,
      _ddsPort = Nothing
    }

-- | The name of the server on the DocumentDB source endpoint.
ddsServerName :: Lens' DocDBSettings (Maybe Text)
ddsServerName = lens _ddsServerName (\s a -> s {_ddsServerName = a})

-- | The user name you use to access the DocumentDB source endpoint.
ddsUsername :: Lens' DocDBSettings (Maybe Text)
ddsUsername = lens _ddsUsername (\s a -> s {_ddsUsername = a})

-- | The AWS KMS key identifier that is used to encrypt the content on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
ddsKMSKeyId :: Lens' DocDBSettings (Maybe Text)
ddsKMSKeyId = lens _ddsKMSKeyId (\s a -> s {_ddsKMSKeyId = a})

-- | The password for the user account you use to access the DocumentDB source endpoint.
ddsPassword :: Lens' DocDBSettings (Maybe Text)
ddsPassword = lens _ddsPassword (\s a -> s {_ddsPassword = a}) . mapping _Sensitive

-- | Specifies either document or table mode.  Default value is @"none"@ . Specify @"none"@ to use document mode. Specify @"one"@ to use table mode.
ddsNestingLevel :: Lens' DocDBSettings (Maybe NestingLevelValue)
ddsNestingLevel = lens _ddsNestingLevel (\s a -> s {_ddsNestingLevel = a})

-- | The database name on the DocumentDB source endpoint.
ddsDatabaseName :: Lens' DocDBSettings (Maybe Text)
ddsDatabaseName = lens _ddsDatabaseName (\s a -> s {_ddsDatabaseName = a})

-- | Indicates the number of documents to preview to determine the document organization. Use this setting when @NestingLevel@ is set to @"one"@ .  Must be a positive value greater than @0@ . Default value is @1000@ .
ddsDocsToInvestigate :: Lens' DocDBSettings (Maybe Int)
ddsDocsToInvestigate = lens _ddsDocsToInvestigate (\s a -> s {_ddsDocsToInvestigate = a})

-- | Specifies the document ID. Use this setting when @NestingLevel@ is set to @"none"@ .  Default value is @"false"@ .
ddsExtractDocId :: Lens' DocDBSettings (Maybe Bool)
ddsExtractDocId = lens _ddsExtractDocId (\s a -> s {_ddsExtractDocId = a})

-- | The port value for the DocumentDB source endpoint.
ddsPort :: Lens' DocDBSettings (Maybe Int)
ddsPort = lens _ddsPort (\s a -> s {_ddsPort = a})

instance FromJSON DocDBSettings where
  parseJSON =
    withObject
      "DocDBSettings"
      ( \x ->
          DocDBSettings'
            <$> (x .:? "ServerName")
            <*> (x .:? "Username")
            <*> (x .:? "KmsKeyId")
            <*> (x .:? "Password")
            <*> (x .:? "NestingLevel")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "DocsToInvestigate")
            <*> (x .:? "ExtractDocId")
            <*> (x .:? "Port")
      )

instance Hashable DocDBSettings

instance NFData DocDBSettings

instance ToJSON DocDBSettings where
  toJSON DocDBSettings' {..} =
    object
      ( catMaybes
          [ ("ServerName" .=) <$> _ddsServerName,
            ("Username" .=) <$> _ddsUsername,
            ("KmsKeyId" .=) <$> _ddsKMSKeyId,
            ("Password" .=) <$> _ddsPassword,
            ("NestingLevel" .=) <$> _ddsNestingLevel,
            ("DatabaseName" .=) <$> _ddsDatabaseName,
            ("DocsToInvestigate" .=) <$> _ddsDocsToInvestigate,
            ("ExtractDocId" .=) <$> _ddsExtractDocId,
            ("Port" .=) <$> _ddsPort
          ]
      )
