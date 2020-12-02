{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.BootstrapActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.BootstrapActionConfig where

import Network.AWS.EMR.Types.ScriptBootstrapActionConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration of a bootstrap action.
--
--
--
-- /See:/ 'bootstrapActionConfig' smart constructor.
data BootstrapActionConfig = BootstrapActionConfig'
  { _bacName ::
      !Text,
    _bacScriptBootstrapAction ::
      !ScriptBootstrapActionConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BootstrapActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bacName' - The name of the bootstrap action.
--
-- * 'bacScriptBootstrapAction' - The script run by the bootstrap action.
bootstrapActionConfig ::
  -- | 'bacName'
  Text ->
  -- | 'bacScriptBootstrapAction'
  ScriptBootstrapActionConfig ->
  BootstrapActionConfig
bootstrapActionConfig pName_ pScriptBootstrapAction_ =
  BootstrapActionConfig'
    { _bacName = pName_,
      _bacScriptBootstrapAction = pScriptBootstrapAction_
    }

-- | The name of the bootstrap action.
bacName :: Lens' BootstrapActionConfig Text
bacName = lens _bacName (\s a -> s {_bacName = a})

-- | The script run by the bootstrap action.
bacScriptBootstrapAction :: Lens' BootstrapActionConfig ScriptBootstrapActionConfig
bacScriptBootstrapAction = lens _bacScriptBootstrapAction (\s a -> s {_bacScriptBootstrapAction = a})

instance Hashable BootstrapActionConfig

instance NFData BootstrapActionConfig

instance ToJSON BootstrapActionConfig where
  toJSON BootstrapActionConfig' {..} =
    object
      ( catMaybes
          [ Just ("Name" .= _bacName),
            Just ("ScriptBootstrapAction" .= _bacScriptBootstrapAction)
          ]
      )
