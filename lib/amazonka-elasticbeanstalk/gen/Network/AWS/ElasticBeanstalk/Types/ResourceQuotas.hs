{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ResourceQuotas
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ResourceQuotas where

import Network.AWS.ElasticBeanstalk.Types.ResourceQuota
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A set of per-resource AWS Elastic Beanstalk quotas associated with an AWS account. They reflect Elastic Beanstalk resource limits for this account.
--
--
--
-- /See:/ 'resourceQuotas' smart constructor.
data ResourceQuotas = ResourceQuotas'
  { _rqApplicationQuota ::
      !(Maybe ResourceQuota),
    _rqCustomPlatformQuota :: !(Maybe ResourceQuota),
    _rqApplicationVersionQuota :: !(Maybe ResourceQuota),
    _rqEnvironmentQuota :: !(Maybe ResourceQuota),
    _rqConfigurationTemplateQuota :: !(Maybe ResourceQuota)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceQuotas' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rqApplicationQuota' - The quota for applications in the AWS account.
--
-- * 'rqCustomPlatformQuota' - The quota for custom platforms in the AWS account.
--
-- * 'rqApplicationVersionQuota' - The quota for application versions in the AWS account.
--
-- * 'rqEnvironmentQuota' - The quota for environments in the AWS account.
--
-- * 'rqConfigurationTemplateQuota' - The quota for configuration templates in the AWS account.
resourceQuotas ::
  ResourceQuotas
resourceQuotas =
  ResourceQuotas'
    { _rqApplicationQuota = Nothing,
      _rqCustomPlatformQuota = Nothing,
      _rqApplicationVersionQuota = Nothing,
      _rqEnvironmentQuota = Nothing,
      _rqConfigurationTemplateQuota = Nothing
    }

-- | The quota for applications in the AWS account.
rqApplicationQuota :: Lens' ResourceQuotas (Maybe ResourceQuota)
rqApplicationQuota = lens _rqApplicationQuota (\s a -> s {_rqApplicationQuota = a})

-- | The quota for custom platforms in the AWS account.
rqCustomPlatformQuota :: Lens' ResourceQuotas (Maybe ResourceQuota)
rqCustomPlatformQuota = lens _rqCustomPlatformQuota (\s a -> s {_rqCustomPlatformQuota = a})

-- | The quota for application versions in the AWS account.
rqApplicationVersionQuota :: Lens' ResourceQuotas (Maybe ResourceQuota)
rqApplicationVersionQuota = lens _rqApplicationVersionQuota (\s a -> s {_rqApplicationVersionQuota = a})

-- | The quota for environments in the AWS account.
rqEnvironmentQuota :: Lens' ResourceQuotas (Maybe ResourceQuota)
rqEnvironmentQuota = lens _rqEnvironmentQuota (\s a -> s {_rqEnvironmentQuota = a})

-- | The quota for configuration templates in the AWS account.
rqConfigurationTemplateQuota :: Lens' ResourceQuotas (Maybe ResourceQuota)
rqConfigurationTemplateQuota = lens _rqConfigurationTemplateQuota (\s a -> s {_rqConfigurationTemplateQuota = a})

instance FromXML ResourceQuotas where
  parseXML x =
    ResourceQuotas'
      <$> (x .@? "ApplicationQuota")
      <*> (x .@? "CustomPlatformQuota")
      <*> (x .@? "ApplicationVersionQuota")
      <*> (x .@? "EnvironmentQuota")
      <*> (x .@? "ConfigurationTemplateQuota")

instance Hashable ResourceQuotas

instance NFData ResourceQuotas
