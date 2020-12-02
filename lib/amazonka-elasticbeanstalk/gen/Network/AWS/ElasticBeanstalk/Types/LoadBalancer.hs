{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LoadBalancer where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a LoadBalancer.
--
--
--
-- /See:/ 'loadBalancer' smart constructor.
newtype LoadBalancer = LoadBalancer' {_lbName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbName' - The name of the LoadBalancer.
loadBalancer ::
  LoadBalancer
loadBalancer = LoadBalancer' {_lbName = Nothing}

-- | The name of the LoadBalancer.
lbName :: Lens' LoadBalancer (Maybe Text)
lbName = lens _lbName (\s a -> s {_lbName = a})

instance FromXML LoadBalancer where
  parseXML x = LoadBalancer' <$> (x .@? "Name")

instance Hashable LoadBalancer

instance NFData LoadBalancer
