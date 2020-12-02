{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentLink where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A link to another environment, defined in the environment's manifest. Links provide connection information in system properties that can be used to connect to another environment in the same group. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
--
--
-- /See:/ 'environmentLink' smart constructor.
data EnvironmentLink = EnvironmentLink'
  { _elLinkName ::
      !(Maybe Text),
    _elEnvironmentName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elLinkName' - The name of the link.
--
-- * 'elEnvironmentName' - The name of the linked environment (the dependency).
environmentLink ::
  EnvironmentLink
environmentLink =
  EnvironmentLink'
    { _elLinkName = Nothing,
      _elEnvironmentName = Nothing
    }

-- | The name of the link.
elLinkName :: Lens' EnvironmentLink (Maybe Text)
elLinkName = lens _elLinkName (\s a -> s {_elLinkName = a})

-- | The name of the linked environment (the dependency).
elEnvironmentName :: Lens' EnvironmentLink (Maybe Text)
elEnvironmentName = lens _elEnvironmentName (\s a -> s {_elEnvironmentName = a})

instance FromXML EnvironmentLink where
  parseXML x =
    EnvironmentLink'
      <$> (x .@? "LinkName") <*> (x .@? "EnvironmentName")

instance Hashable EnvironmentLink

instance NFData EnvironmentLink
