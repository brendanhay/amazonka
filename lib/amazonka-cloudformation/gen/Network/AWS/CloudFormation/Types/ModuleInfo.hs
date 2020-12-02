{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ModuleInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ModuleInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
--
-- For more information on modules, see <AWSCloudFormation/latest/UserGuide/modules.html Using modules to encapsulate and reuse resource configurations> in the /CloudFormation User Guide/ .
--
--
-- /See:/ 'moduleInfo' smart constructor.
data ModuleInfo = ModuleInfo'
  { _miTypeHierarchy :: !(Maybe Text),
    _miLogicalIdHierarchy :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModuleInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miTypeHierarchy' - A concantenated list of the the module type or types containing the resource. Module types are listed starting with the inner-most nested module, and separated by @/@ . In the following example, the resource was created from a module of type @AWS::First::Example::MODULE@ , that is nested inside a parent module of type @AWS::Second::Example::MODULE@ . @AWS::First::Example::MODULE/AWS::Second::Example::MODULE@
--
-- * 'miLogicalIdHierarchy' - A concantenated list of the logical IDs of the module or modules containing the resource. Modules are listed starting with the inner-most nested module, and separated by @/@ . In the following example, the resource was created from a module, @moduleA@ , that is nested inside a parent module, @moduleB@ . @moduleA/moduleB@  For more information, see <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module> in the /CloudFormation User Guide/ .
moduleInfo ::
  ModuleInfo
moduleInfo =
  ModuleInfo'
    { _miTypeHierarchy = Nothing,
      _miLogicalIdHierarchy = Nothing
    }

-- | A concantenated list of the the module type or types containing the resource. Module types are listed starting with the inner-most nested module, and separated by @/@ . In the following example, the resource was created from a module of type @AWS::First::Example::MODULE@ , that is nested inside a parent module of type @AWS::Second::Example::MODULE@ . @AWS::First::Example::MODULE/AWS::Second::Example::MODULE@
miTypeHierarchy :: Lens' ModuleInfo (Maybe Text)
miTypeHierarchy = lens _miTypeHierarchy (\s a -> s {_miTypeHierarchy = a})

-- | A concantenated list of the logical IDs of the module or modules containing the resource. Modules are listed starting with the inner-most nested module, and separated by @/@ . In the following example, the resource was created from a module, @moduleA@ , that is nested inside a parent module, @moduleB@ . @moduleA/moduleB@  For more information, see <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module> in the /CloudFormation User Guide/ .
miLogicalIdHierarchy :: Lens' ModuleInfo (Maybe Text)
miLogicalIdHierarchy = lens _miLogicalIdHierarchy (\s a -> s {_miLogicalIdHierarchy = a})

instance FromXML ModuleInfo where
  parseXML x =
    ModuleInfo'
      <$> (x .@? "TypeHierarchy") <*> (x .@? "LogicalIdHierarchy")

instance Hashable ModuleInfo

instance NFData ModuleInfo
