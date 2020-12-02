{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.MinimumHealthyHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.MinimumHealthyHosts where

import Network.AWS.CodeDeploy.Types.MinimumHealthyHostsType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about minimum healthy instance.
--
--
--
-- /See:/ 'minimumHealthyHosts' smart constructor.
data MinimumHealthyHosts = MinimumHealthyHosts'
  { _mhhValue ::
      !(Maybe Int),
    _mhhType :: !(Maybe MinimumHealthyHostsType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MinimumHealthyHosts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mhhValue' - The minimum healthy instance value.
--
-- * 'mhhType' - The minimum healthy instance type:     * @HOST_COUNT@ : The minimum number of healthy instances as an absolute value.     * @FLEET_PERCENT@ : The minimum number of healthy instances as a percentage of the total number of instances in the deployment. In an example of nine instances, if a HOST_COUNT of six is specified, deploy to up to three instances at a time. The deployment is successful if six or more instances are deployed to successfully. Otherwise, the deployment fails. If a FLEET_PERCENT of 40 is specified, deploy to up to five instances at a time. The deployment is successful if four or more instances are deployed to successfully. Otherwise, the deployment fails. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/instances-health.html AWS CodeDeploy Instance Health> in the /AWS CodeDeploy User Guide/ .
minimumHealthyHosts ::
  MinimumHealthyHosts
minimumHealthyHosts =
  MinimumHealthyHosts' {_mhhValue = Nothing, _mhhType = Nothing}

-- | The minimum healthy instance value.
mhhValue :: Lens' MinimumHealthyHosts (Maybe Int)
mhhValue = lens _mhhValue (\s a -> s {_mhhValue = a})

-- | The minimum healthy instance type:     * @HOST_COUNT@ : The minimum number of healthy instances as an absolute value.     * @FLEET_PERCENT@ : The minimum number of healthy instances as a percentage of the total number of instances in the deployment. In an example of nine instances, if a HOST_COUNT of six is specified, deploy to up to three instances at a time. The deployment is successful if six or more instances are deployed to successfully. Otherwise, the deployment fails. If a FLEET_PERCENT of 40 is specified, deploy to up to five instances at a time. The deployment is successful if four or more instances are deployed to successfully. Otherwise, the deployment fails. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/instances-health.html AWS CodeDeploy Instance Health> in the /AWS CodeDeploy User Guide/ .
mhhType :: Lens' MinimumHealthyHosts (Maybe MinimumHealthyHostsType)
mhhType = lens _mhhType (\s a -> s {_mhhType = a})

instance FromJSON MinimumHealthyHosts where
  parseJSON =
    withObject
      "MinimumHealthyHosts"
      ( \x ->
          MinimumHealthyHosts' <$> (x .:? "value") <*> (x .:? "type")
      )

instance Hashable MinimumHealthyHosts

instance NFData MinimumHealthyHosts

instance ToJSON MinimumHealthyHosts where
  toJSON MinimumHealthyHosts' {..} =
    object
      (catMaybes [("value" .=) <$> _mhhValue, ("type" .=) <$> _mhhType])
