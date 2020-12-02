{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Parameter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Parameter data type.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pParameterValue :: !(Maybe Text),
    _pResolvedValue :: !(Maybe Text),
    _pParameterKey :: !(Maybe Text),
    _pUsePreviousValue :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pParameterValue' - The input value associated with the parameter.
--
-- * 'pResolvedValue' - Read-only. The value that corresponds to a Systems Manager parameter key. This field is returned only for <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types @SSM@ parameter types> in the template.
--
-- * 'pParameterKey' - The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation uses the default value that is specified in your template.
--
-- * 'pUsePreviousValue' - During a stack update, use the existing parameter value that the stack is using for a given parameter key. If you specify @true@ , do not specify a parameter value.
parameter ::
  Parameter
parameter =
  Parameter'
    { _pParameterValue = Nothing,
      _pResolvedValue = Nothing,
      _pParameterKey = Nothing,
      _pUsePreviousValue = Nothing
    }

-- | The input value associated with the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s {_pParameterValue = a})

-- | Read-only. The value that corresponds to a Systems Manager parameter key. This field is returned only for <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/parameters-section-structure.html#aws-ssm-parameter-types @SSM@ parameter types> in the template.
pResolvedValue :: Lens' Parameter (Maybe Text)
pResolvedValue = lens _pResolvedValue (\s a -> s {_pResolvedValue = a})

-- | The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation uses the default value that is specified in your template.
pParameterKey :: Lens' Parameter (Maybe Text)
pParameterKey = lens _pParameterKey (\s a -> s {_pParameterKey = a})

-- | During a stack update, use the existing parameter value that the stack is using for a given parameter key. If you specify @true@ , do not specify a parameter value.
pUsePreviousValue :: Lens' Parameter (Maybe Bool)
pUsePreviousValue = lens _pUsePreviousValue (\s a -> s {_pUsePreviousValue = a})

instance FromXML Parameter where
  parseXML x =
    Parameter'
      <$> (x .@? "ParameterValue")
      <*> (x .@? "ResolvedValue")
      <*> (x .@? "ParameterKey")
      <*> (x .@? "UsePreviousValue")

instance Hashable Parameter

instance NFData Parameter

instance ToQuery Parameter where
  toQuery Parameter' {..} =
    mconcat
      [ "ParameterValue" =: _pParameterValue,
        "ResolvedValue" =: _pResolvedValue,
        "ParameterKey" =: _pParameterKey,
        "UsePreviousValue" =: _pUsePreviousValue
      ]
