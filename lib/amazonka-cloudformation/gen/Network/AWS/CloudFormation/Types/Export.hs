{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Export
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Export where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @Export@ structure describes the exported output values for a stack.
--
--
--
-- /See:/ 'export'' smart constructor.
data Export = Export'
  { _eValue :: !(Maybe Text),
    _eExportingStackId :: !(Maybe Text),
    _eName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Export' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eValue' - The value of the exported output, such as a resource physical ID. This value is defined in the @Export@ field in the associated stack's @Outputs@ section.
--
-- * 'eExportingStackId' - The stack that contains the exported output name and value.
--
-- * 'eName' - The name of exported output value. Use this name and the @Fn::ImportValue@ function to import the associated value into other stacks. The name is defined in the @Export@ field in the associated stack's @Outputs@ section.
export' ::
  Export
export' =
  Export'
    { _eValue = Nothing,
      _eExportingStackId = Nothing,
      _eName = Nothing
    }

-- | The value of the exported output, such as a resource physical ID. This value is defined in the @Export@ field in the associated stack's @Outputs@ section.
eValue :: Lens' Export (Maybe Text)
eValue = lens _eValue (\s a -> s {_eValue = a})

-- | The stack that contains the exported output name and value.
eExportingStackId :: Lens' Export (Maybe Text)
eExportingStackId = lens _eExportingStackId (\s a -> s {_eExportingStackId = a})

-- | The name of exported output value. Use this name and the @Fn::ImportValue@ function to import the associated value into other stacks. The name is defined in the @Export@ field in the associated stack's @Outputs@ section.
eName :: Lens' Export (Maybe Text)
eName = lens _eName (\s a -> s {_eName = a})

instance FromXML Export where
  parseXML x =
    Export'
      <$> (x .@? "Value") <*> (x .@? "ExportingStackId") <*> (x .@? "Name")

instance Hashable Export

instance NFData Export
