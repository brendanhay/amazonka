{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ParameterDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ParameterDeclaration where

import Network.AWS.CloudFormation.Types.ParameterConstraints
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The ParameterDeclaration data type.
--
--
--
-- /See:/ 'parameterDeclaration' smart constructor.
data ParameterDeclaration = ParameterDeclaration'
  { _pdParameterKey ::
      !(Maybe Text),
    _pdParameterType :: !(Maybe Text),
    _pdParameterConstraints ::
      !(Maybe ParameterConstraints),
    _pdDefaultValue :: !(Maybe Text),
    _pdNoEcho :: !(Maybe Bool),
    _pdDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterDeclaration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdParameterKey' - The name that is associated with the parameter.
--
-- * 'pdParameterType' - The type of parameter.
--
-- * 'pdParameterConstraints' - The criteria that AWS CloudFormation uses to validate parameter values.
--
-- * 'pdDefaultValue' - The default value of the parameter.
--
-- * 'pdNoEcho' - Flag that indicates whether the parameter value is shown as plain text in logs and in the AWS Management Console.
--
-- * 'pdDescription' - The description that is associate with the parameter.
parameterDeclaration ::
  ParameterDeclaration
parameterDeclaration =
  ParameterDeclaration'
    { _pdParameterKey = Nothing,
      _pdParameterType = Nothing,
      _pdParameterConstraints = Nothing,
      _pdDefaultValue = Nothing,
      _pdNoEcho = Nothing,
      _pdDescription = Nothing
    }

-- | The name that is associated with the parameter.
pdParameterKey :: Lens' ParameterDeclaration (Maybe Text)
pdParameterKey = lens _pdParameterKey (\s a -> s {_pdParameterKey = a})

-- | The type of parameter.
pdParameterType :: Lens' ParameterDeclaration (Maybe Text)
pdParameterType = lens _pdParameterType (\s a -> s {_pdParameterType = a})

-- | The criteria that AWS CloudFormation uses to validate parameter values.
pdParameterConstraints :: Lens' ParameterDeclaration (Maybe ParameterConstraints)
pdParameterConstraints = lens _pdParameterConstraints (\s a -> s {_pdParameterConstraints = a})

-- | The default value of the parameter.
pdDefaultValue :: Lens' ParameterDeclaration (Maybe Text)
pdDefaultValue = lens _pdDefaultValue (\s a -> s {_pdDefaultValue = a})

-- | Flag that indicates whether the parameter value is shown as plain text in logs and in the AWS Management Console.
pdNoEcho :: Lens' ParameterDeclaration (Maybe Bool)
pdNoEcho = lens _pdNoEcho (\s a -> s {_pdNoEcho = a})

-- | The description that is associate with the parameter.
pdDescription :: Lens' ParameterDeclaration (Maybe Text)
pdDescription = lens _pdDescription (\s a -> s {_pdDescription = a})

instance FromXML ParameterDeclaration where
  parseXML x =
    ParameterDeclaration'
      <$> (x .@? "ParameterKey")
      <*> (x .@? "ParameterType")
      <*> (x .@? "ParameterConstraints")
      <*> (x .@? "DefaultValue")
      <*> (x .@? "NoEcho")
      <*> (x .@? "Description")

instance Hashable ParameterDeclaration

instance NFData ParameterDeclaration
