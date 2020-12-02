{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig where

import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The resource lifecycle configuration for an application. Defines lifecycle settings for resources that belong to the application, and the service role that AWS Elastic Beanstalk assumes in order to apply lifecycle settings. The version lifecycle configuration defines lifecycle settings for application versions.
--
--
--
-- /See:/ 'applicationResourceLifecycleConfig' smart constructor.
data ApplicationResourceLifecycleConfig = ApplicationResourceLifecycleConfig'
  { _arlcVersionLifecycleConfig ::
      !( Maybe
           ApplicationVersionLifecycleConfig
       ),
    _arlcServiceRole ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationResourceLifecycleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arlcVersionLifecycleConfig' - Defines lifecycle settings for application versions.
--
-- * 'arlcServiceRole' - The ARN of an IAM service role that Elastic Beanstalk has permission to assume. The @ServiceRole@ property is required the first time that you provide a @VersionLifecycleConfig@ for the application in one of the supporting calls (@CreateApplication@ or @UpdateApplicationResourceLifecycle@ ). After you provide it once, in either one of the calls, Elastic Beanstalk persists the Service Role with the application, and you don't need to specify it again in subsequent @UpdateApplicationResourceLifecycle@ calls. You can, however, specify it in subsequent calls to change the Service Role to another value.
applicationResourceLifecycleConfig ::
  ApplicationResourceLifecycleConfig
applicationResourceLifecycleConfig =
  ApplicationResourceLifecycleConfig'
    { _arlcVersionLifecycleConfig =
        Nothing,
      _arlcServiceRole = Nothing
    }

-- | Defines lifecycle settings for application versions.
arlcVersionLifecycleConfig :: Lens' ApplicationResourceLifecycleConfig (Maybe ApplicationVersionLifecycleConfig)
arlcVersionLifecycleConfig = lens _arlcVersionLifecycleConfig (\s a -> s {_arlcVersionLifecycleConfig = a})

-- | The ARN of an IAM service role that Elastic Beanstalk has permission to assume. The @ServiceRole@ property is required the first time that you provide a @VersionLifecycleConfig@ for the application in one of the supporting calls (@CreateApplication@ or @UpdateApplicationResourceLifecycle@ ). After you provide it once, in either one of the calls, Elastic Beanstalk persists the Service Role with the application, and you don't need to specify it again in subsequent @UpdateApplicationResourceLifecycle@ calls. You can, however, specify it in subsequent calls to change the Service Role to another value.
arlcServiceRole :: Lens' ApplicationResourceLifecycleConfig (Maybe Text)
arlcServiceRole = lens _arlcServiceRole (\s a -> s {_arlcServiceRole = a})

instance FromXML ApplicationResourceLifecycleConfig where
  parseXML x =
    ApplicationResourceLifecycleConfig'
      <$> (x .@? "VersionLifecycleConfig") <*> (x .@? "ServiceRole")

instance Hashable ApplicationResourceLifecycleConfig

instance NFData ApplicationResourceLifecycleConfig

instance ToQuery ApplicationResourceLifecycleConfig where
  toQuery ApplicationResourceLifecycleConfig' {..} =
    mconcat
      [ "VersionLifecycleConfig" =: _arlcVersionLifecycleConfig,
        "ServiceRole" =: _arlcServiceRole
      ]
