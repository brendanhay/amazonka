{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.DeploymentCommand
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.DeploymentCommand where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.DeploymentCommandName
import Network.AWS.Prelude

-- | Used to specify a stack or deployment command.
--
--
--
-- /See:/ 'deploymentCommand' smart constructor.
data DeploymentCommand = DeploymentCommand'
  { _dcArgs ::
      !(Maybe (Map Text ([Text]))),
    _dcName :: !DeploymentCommandName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeploymentCommand' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcArgs' - The arguments of those commands that take arguments. It should be set to a JSON object with the following format: @{"arg_name1" : ["value1", "value2", ...], "arg_name2" : ["value1", "value2", ...], ...}@  The @update_dependencies@ command takes two arguments:     * @upgrade_os_to@ - Specifies the desired Amazon Linux version for instances whose OS you want to upgrade, such as @Amazon Linux 2016.09@ . You must also set the @allow_reboot@ argument to true.     * @allow_reboot@ - Specifies whether to allow AWS OpsWorks Stacks to reboot the instances if necessary, after installing the updates. This argument can be set to either @true@ or @false@ . The default value is @false@ . For example, to upgrade an instance to Amazon Linux 2016.09, set @Args@ to the following. @{ "upgrade_os_to":["Amazon Linux 2016.09"], "allow_reboot":["true"] } @
--
-- * 'dcName' - Specifies the operation. You can specify only one command. For stacks, the following commands are available:     * @execute_recipes@ : Execute one or more recipes. To specify the recipes, set an @Args@ parameter named @recipes@ to the list of recipes to be executed. For example, to execute @phpapp::appsetup@ , set @Args@ to @{"recipes":["phpapp::appsetup"]}@ .     * @install_dependencies@ : Install the stack's dependencies.     * @update_custom_cookbooks@ : Update the stack's custom cookbooks.     * @update_dependencies@ : Update the stack's dependencies. For apps, the following commands are available:     * @deploy@ : Deploy an app. Ruby on Rails apps have an optional @Args@ parameter named @migrate@ . Set @Args@ to {"migrate":["true"]} to migrate the database. The default setting is {"migrate":["false"]}.     * @rollback@ Roll the app back to the previous version. When you update an app, AWS OpsWorks Stacks stores the previous version, up to a maximum of five versions. You can use this command to roll an app back as many as four versions.     * @start@ : Start the app's web or application server.     * @stop@ : Stop the app's web or application server.     * @restart@ : Restart the app's web or application server.     * @undeploy@ : Undeploy the app.
deploymentCommand ::
  -- | 'dcName'
  DeploymentCommandName ->
  DeploymentCommand
deploymentCommand pName_ =
  DeploymentCommand' {_dcArgs = Nothing, _dcName = pName_}

-- | The arguments of those commands that take arguments. It should be set to a JSON object with the following format: @{"arg_name1" : ["value1", "value2", ...], "arg_name2" : ["value1", "value2", ...], ...}@  The @update_dependencies@ command takes two arguments:     * @upgrade_os_to@ - Specifies the desired Amazon Linux version for instances whose OS you want to upgrade, such as @Amazon Linux 2016.09@ . You must also set the @allow_reboot@ argument to true.     * @allow_reboot@ - Specifies whether to allow AWS OpsWorks Stacks to reboot the instances if necessary, after installing the updates. This argument can be set to either @true@ or @false@ . The default value is @false@ . For example, to upgrade an instance to Amazon Linux 2016.09, set @Args@ to the following. @{ "upgrade_os_to":["Amazon Linux 2016.09"], "allow_reboot":["true"] } @
dcArgs :: Lens' DeploymentCommand (HashMap Text ([Text]))
dcArgs = lens _dcArgs (\s a -> s {_dcArgs = a}) . _Default . _Map

-- | Specifies the operation. You can specify only one command. For stacks, the following commands are available:     * @execute_recipes@ : Execute one or more recipes. To specify the recipes, set an @Args@ parameter named @recipes@ to the list of recipes to be executed. For example, to execute @phpapp::appsetup@ , set @Args@ to @{"recipes":["phpapp::appsetup"]}@ .     * @install_dependencies@ : Install the stack's dependencies.     * @update_custom_cookbooks@ : Update the stack's custom cookbooks.     * @update_dependencies@ : Update the stack's dependencies. For apps, the following commands are available:     * @deploy@ : Deploy an app. Ruby on Rails apps have an optional @Args@ parameter named @migrate@ . Set @Args@ to {"migrate":["true"]} to migrate the database. The default setting is {"migrate":["false"]}.     * @rollback@ Roll the app back to the previous version. When you update an app, AWS OpsWorks Stacks stores the previous version, up to a maximum of five versions. You can use this command to roll an app back as many as four versions.     * @start@ : Start the app's web or application server.     * @stop@ : Stop the app's web or application server.     * @restart@ : Restart the app's web or application server.     * @undeploy@ : Undeploy the app.
dcName :: Lens' DeploymentCommand DeploymentCommandName
dcName = lens _dcName (\s a -> s {_dcName = a})

instance FromJSON DeploymentCommand where
  parseJSON =
    withObject
      "DeploymentCommand"
      ( \x ->
          DeploymentCommand' <$> (x .:? "Args" .!= mempty) <*> (x .: "Name")
      )

instance Hashable DeploymentCommand

instance NFData DeploymentCommand

instance ToJSON DeploymentCommand where
  toJSON DeploymentCommand' {..} =
    object
      (catMaybes [("Args" .=) <$> _dcArgs, Just ("Name" .= _dcName)])
