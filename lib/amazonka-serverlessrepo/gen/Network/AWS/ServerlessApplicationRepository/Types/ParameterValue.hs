{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ParameterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ParameterValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Parameter value of the application.
--
--
--
-- /See:/ 'parameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { _pvValue :: !Text,
    _pvName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvValue' - The input value associated with the parameter.
--
-- * 'pvName' - The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation  uses the default value that is specified in your template.
parameterValue ::
  -- | 'pvValue'
  Text ->
  -- | 'pvName'
  Text ->
  ParameterValue
parameterValue pValue_ pName_ =
  ParameterValue' {_pvValue = pValue_, _pvName = pName_}

-- | The input value associated with the parameter.
pvValue :: Lens' ParameterValue Text
pvValue = lens _pvValue (\s a -> s {_pvValue = a})

-- | The key associated with the parameter. If you don't specify a key and value for a particular parameter, AWS CloudFormation  uses the default value that is specified in your template.
pvName :: Lens' ParameterValue Text
pvName = lens _pvName (\s a -> s {_pvName = a})

instance Hashable ParameterValue

instance NFData ParameterValue

instance ToJSON ParameterValue where
  toJSON ParameterValue' {..} =
    object
      (catMaybes [Just ("value" .= _pvValue), Just ("name" .= _pvName)])
