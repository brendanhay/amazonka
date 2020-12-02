{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ResourceQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ResourceQuota where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The AWS Elastic Beanstalk quota information for a single resource type in an AWS account. It reflects the resource's limits for this account.
--
--
--
-- /See:/ 'resourceQuota' smart constructor.
newtype ResourceQuota = ResourceQuota' {_rqMaximum :: Maybe Int}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceQuota' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rqMaximum' - The maximum number of instances of this Elastic Beanstalk resource type that an AWS account can use.
resourceQuota ::
  ResourceQuota
resourceQuota = ResourceQuota' {_rqMaximum = Nothing}

-- | The maximum number of instances of this Elastic Beanstalk resource type that an AWS account can use.
rqMaximum :: Lens' ResourceQuota (Maybe Int)
rqMaximum = lens _rqMaximum (\s a -> s {_rqMaximum = a})

instance FromXML ResourceQuota where
  parseXML x = ResourceQuota' <$> (x .@? "Maximum")

instance Hashable ResourceQuota

instance NFData ResourceQuota
