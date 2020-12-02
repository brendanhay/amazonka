{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationDescription where

import Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the properties of an application.
--
--
--
-- /See:/ 'applicationDescription' smart constructor.
data ApplicationDescription = ApplicationDescription'
  { _adApplicationARN ::
      !(Maybe Text),
    _adVersions :: !(Maybe [Text]),
    _adDateUpdated :: !(Maybe ISO8601),
    _adDateCreated :: !(Maybe ISO8601),
    _adApplicationName :: !(Maybe Text),
    _adConfigurationTemplates :: !(Maybe [Text]),
    _adResourceLifecycleConfig ::
      !(Maybe ApplicationResourceLifecycleConfig),
    _adDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adApplicationARN' - The Amazon Resource Name (ARN) of the application.
--
-- * 'adVersions' - The names of the versions for this application.
--
-- * 'adDateUpdated' - The date when the application was last modified.
--
-- * 'adDateCreated' - The date when the application was created.
--
-- * 'adApplicationName' - The name of the application.
--
-- * 'adConfigurationTemplates' - The names of the configuration templates associated with this application.
--
-- * 'adResourceLifecycleConfig' - The lifecycle settings for the application.
--
-- * 'adDescription' - User-defined description of the application.
applicationDescription ::
  ApplicationDescription
applicationDescription =
  ApplicationDescription'
    { _adApplicationARN = Nothing,
      _adVersions = Nothing,
      _adDateUpdated = Nothing,
      _adDateCreated = Nothing,
      _adApplicationName = Nothing,
      _adConfigurationTemplates = Nothing,
      _adResourceLifecycleConfig = Nothing,
      _adDescription = Nothing
    }

-- | The Amazon Resource Name (ARN) of the application.
adApplicationARN :: Lens' ApplicationDescription (Maybe Text)
adApplicationARN = lens _adApplicationARN (\s a -> s {_adApplicationARN = a})

-- | The names of the versions for this application.
adVersions :: Lens' ApplicationDescription [Text]
adVersions = lens _adVersions (\s a -> s {_adVersions = a}) . _Default . _Coerce

-- | The date when the application was last modified.
adDateUpdated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateUpdated = lens _adDateUpdated (\s a -> s {_adDateUpdated = a}) . mapping _Time

-- | The date when the application was created.
adDateCreated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateCreated = lens _adDateCreated (\s a -> s {_adDateCreated = a}) . mapping _Time

-- | The name of the application.
adApplicationName :: Lens' ApplicationDescription (Maybe Text)
adApplicationName = lens _adApplicationName (\s a -> s {_adApplicationName = a})

-- | The names of the configuration templates associated with this application.
adConfigurationTemplates :: Lens' ApplicationDescription [Text]
adConfigurationTemplates = lens _adConfigurationTemplates (\s a -> s {_adConfigurationTemplates = a}) . _Default . _Coerce

-- | The lifecycle settings for the application.
adResourceLifecycleConfig :: Lens' ApplicationDescription (Maybe ApplicationResourceLifecycleConfig)
adResourceLifecycleConfig = lens _adResourceLifecycleConfig (\s a -> s {_adResourceLifecycleConfig = a})

-- | User-defined description of the application.
adDescription :: Lens' ApplicationDescription (Maybe Text)
adDescription = lens _adDescription (\s a -> s {_adDescription = a})

instance FromXML ApplicationDescription where
  parseXML x =
    ApplicationDescription'
      <$> (x .@? "ApplicationArn")
      <*> (x .@? "Versions" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "DateUpdated")
      <*> (x .@? "DateCreated")
      <*> (x .@? "ApplicationName")
      <*> ( x .@? "ConfigurationTemplates" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "ResourceLifecycleConfig")
      <*> (x .@? "Description")

instance Hashable ApplicationDescription

instance NFData ApplicationDescription
