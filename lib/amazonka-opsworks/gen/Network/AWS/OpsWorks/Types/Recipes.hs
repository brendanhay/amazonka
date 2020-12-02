{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Recipes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Recipes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | AWS OpsWorks Stacks supports five lifecycle events: __setup__ , __configuration__ , __deploy__ , __undeploy__ , and __shutdown__ . For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. In addition, you can provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. @LayerCustomRecipes@ specifies the custom recipes for a particular layer to be run in response to each of the five events.
--
--
-- To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the .rb extension. For example: phpapp2::dbsetup specifies the dbsetup.rb recipe in the repository's phpapp2 folder.
--
--
-- /See:/ 'recipes' smart constructor.
data Recipes = Recipes'
  { _rSetup :: !(Maybe [Text]),
    _rShutdown :: !(Maybe [Text]),
    _rUndeploy :: !(Maybe [Text]),
    _rConfigure :: !(Maybe [Text]),
    _rDeploy :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Recipes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rSetup' - An array of custom recipe names to be run following a @setup@ event.
--
-- * 'rShutdown' - An array of custom recipe names to be run following a @shutdown@ event.
--
-- * 'rUndeploy' - An array of custom recipe names to be run following a @undeploy@ event.
--
-- * 'rConfigure' - An array of custom recipe names to be run following a @configure@ event.
--
-- * 'rDeploy' - An array of custom recipe names to be run following a @deploy@ event.
recipes ::
  Recipes
recipes =
  Recipes'
    { _rSetup = Nothing,
      _rShutdown = Nothing,
      _rUndeploy = Nothing,
      _rConfigure = Nothing,
      _rDeploy = Nothing
    }

-- | An array of custom recipe names to be run following a @setup@ event.
rSetup :: Lens' Recipes [Text]
rSetup = lens _rSetup (\s a -> s {_rSetup = a}) . _Default . _Coerce

-- | An array of custom recipe names to be run following a @shutdown@ event.
rShutdown :: Lens' Recipes [Text]
rShutdown = lens _rShutdown (\s a -> s {_rShutdown = a}) . _Default . _Coerce

-- | An array of custom recipe names to be run following a @undeploy@ event.
rUndeploy :: Lens' Recipes [Text]
rUndeploy = lens _rUndeploy (\s a -> s {_rUndeploy = a}) . _Default . _Coerce

-- | An array of custom recipe names to be run following a @configure@ event.
rConfigure :: Lens' Recipes [Text]
rConfigure = lens _rConfigure (\s a -> s {_rConfigure = a}) . _Default . _Coerce

-- | An array of custom recipe names to be run following a @deploy@ event.
rDeploy :: Lens' Recipes [Text]
rDeploy = lens _rDeploy (\s a -> s {_rDeploy = a}) . _Default . _Coerce

instance FromJSON Recipes where
  parseJSON =
    withObject
      "Recipes"
      ( \x ->
          Recipes'
            <$> (x .:? "Setup" .!= mempty)
            <*> (x .:? "Shutdown" .!= mempty)
            <*> (x .:? "Undeploy" .!= mempty)
            <*> (x .:? "Configure" .!= mempty)
            <*> (x .:? "Deploy" .!= mempty)
      )

instance Hashable Recipes

instance NFData Recipes

instance ToJSON Recipes where
  toJSON Recipes' {..} =
    object
      ( catMaybes
          [ ("Setup" .=) <$> _rSetup,
            ("Shutdown" .=) <$> _rShutdown,
            ("Undeploy" .=) <$> _rUndeploy,
            ("Configure" .=) <$> _rConfigure,
            ("Deploy" .=) <$> _rDeploy
          ]
      )
