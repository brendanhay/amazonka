{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig where

import Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
import Network.AWS.ElasticBeanstalk.Types.MaxCountRule
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The application version lifecycle settings for an application. Defines the rules that Elastic Beanstalk applies to an application's versions in order to avoid hitting the per-region limit for application versions.
--
--
-- When Elastic Beanstalk deletes an application version from its database, you can no longer deploy that version to an environment. The source bundle remains in S3 unless you configure the rule to delete it.
--
--
-- /See:/ 'applicationVersionLifecycleConfig' smart constructor.
data ApplicationVersionLifecycleConfig = ApplicationVersionLifecycleConfig'
  { _avlcMaxAgeRule ::
      !(Maybe MaxAgeRule),
    _avlcMaxCountRule ::
      !(Maybe MaxCountRule)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationVersionLifecycleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avlcMaxAgeRule' - Specify a max age rule to restrict the length of time that application versions are retained for an application.
--
-- * 'avlcMaxCountRule' - Specify a max count rule to restrict the number of application versions that are retained for an application.
applicationVersionLifecycleConfig ::
  ApplicationVersionLifecycleConfig
applicationVersionLifecycleConfig =
  ApplicationVersionLifecycleConfig'
    { _avlcMaxAgeRule = Nothing,
      _avlcMaxCountRule = Nothing
    }

-- | Specify a max age rule to restrict the length of time that application versions are retained for an application.
avlcMaxAgeRule :: Lens' ApplicationVersionLifecycleConfig (Maybe MaxAgeRule)
avlcMaxAgeRule = lens _avlcMaxAgeRule (\s a -> s {_avlcMaxAgeRule = a})

-- | Specify a max count rule to restrict the number of application versions that are retained for an application.
avlcMaxCountRule :: Lens' ApplicationVersionLifecycleConfig (Maybe MaxCountRule)
avlcMaxCountRule = lens _avlcMaxCountRule (\s a -> s {_avlcMaxCountRule = a})

instance FromXML ApplicationVersionLifecycleConfig where
  parseXML x =
    ApplicationVersionLifecycleConfig'
      <$> (x .@? "MaxAgeRule") <*> (x .@? "MaxCountRule")

instance Hashable ApplicationVersionLifecycleConfig

instance NFData ApplicationVersionLifecycleConfig

instance ToQuery ApplicationVersionLifecycleConfig where
  toQuery ApplicationVersionLifecycleConfig' {..} =
    mconcat
      [ "MaxAgeRule" =: _avlcMaxAgeRule,
        "MaxCountRule" =: _avlcMaxCountRule
      ]
