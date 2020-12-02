{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ParameterConstraints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ParameterConstraints where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A set of criteria that AWS CloudFormation uses to validate parameter values. Although other constraints might be defined in the stack template, AWS CloudFormation returns only the @AllowedValues@ property.
--
--
--
-- /See:/ 'parameterConstraints' smart constructor.
newtype ParameterConstraints = ParameterConstraints'
  { _pcAllowedValues ::
      Maybe [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterConstraints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcAllowedValues' - A list of values that are permitted for a parameter.
parameterConstraints ::
  ParameterConstraints
parameterConstraints =
  ParameterConstraints' {_pcAllowedValues = Nothing}

-- | A list of values that are permitted for a parameter.
pcAllowedValues :: Lens' ParameterConstraints [Text]
pcAllowedValues = lens _pcAllowedValues (\s a -> s {_pcAllowedValues = a}) . _Default . _Coerce

instance FromXML ParameterConstraints where
  parseXML x =
    ParameterConstraints'
      <$> (x .@? "AllowedValues" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable ParameterConstraints

instance NFData ParameterConstraints
