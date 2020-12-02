{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Application
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Application where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | With Amazon EMR release version 4.0 and later, the only accepted parameter is the application name. To pass arguments to applications, you use configuration classifications specified using configuration JSON objects. For more information, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html Configuring Applications> .
--
--
-- With earlier Amazon EMR releases, the application is any Amazon or third-party software that you can add to the cluster. This structure contains a list of strings that indicates the software to use with the cluster and accepts a user argument list. Amazon EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action argument.
--
--
-- /See:/ 'application' smart constructor.
data Application = Application'
  { _aArgs :: !(Maybe [Text]),
    _aAdditionalInfo :: !(Maybe (Map Text (Text))),
    _aName :: !(Maybe Text),
    _aVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Application' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aArgs' - Arguments for Amazon EMR to pass to the application.
--
-- * 'aAdditionalInfo' - This option is for advanced users only. This is meta information about third-party applications that third-party vendors use for testing purposes.
--
-- * 'aName' - The name of the application.
--
-- * 'aVersion' - The version of the application.
application ::
  Application
application =
  Application'
    { _aArgs = Nothing,
      _aAdditionalInfo = Nothing,
      _aName = Nothing,
      _aVersion = Nothing
    }

-- | Arguments for Amazon EMR to pass to the application.
aArgs :: Lens' Application [Text]
aArgs = lens _aArgs (\s a -> s {_aArgs = a}) . _Default . _Coerce

-- | This option is for advanced users only. This is meta information about third-party applications that third-party vendors use for testing purposes.
aAdditionalInfo :: Lens' Application (HashMap Text (Text))
aAdditionalInfo = lens _aAdditionalInfo (\s a -> s {_aAdditionalInfo = a}) . _Default . _Map

-- | The name of the application.
aName :: Lens' Application (Maybe Text)
aName = lens _aName (\s a -> s {_aName = a})

-- | The version of the application.
aVersion :: Lens' Application (Maybe Text)
aVersion = lens _aVersion (\s a -> s {_aVersion = a})

instance FromJSON Application where
  parseJSON =
    withObject
      "Application"
      ( \x ->
          Application'
            <$> (x .:? "Args" .!= mempty)
            <*> (x .:? "AdditionalInfo" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "Version")
      )

instance Hashable Application

instance NFData Application

instance ToJSON Application where
  toJSON Application' {..} =
    object
      ( catMaybes
          [ ("Args" .=) <$> _aArgs,
            ("AdditionalInfo" .=) <$> _aAdditionalInfo,
            ("Name" .=) <$> _aName,
            ("Version" .=) <$> _aVersion
          ]
      )
