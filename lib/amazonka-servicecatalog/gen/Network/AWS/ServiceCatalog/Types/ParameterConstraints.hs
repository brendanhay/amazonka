{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ParameterConstraints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ParameterConstraints where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The constraints that the administrator has put on the parameter.
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
-- * 'pcAllowedValues' - The values that the administrator has allowed for the parameter.
parameterConstraints ::
  ParameterConstraints
parameterConstraints =
  ParameterConstraints' {_pcAllowedValues = Nothing}

-- | The values that the administrator has allowed for the parameter.
pcAllowedValues :: Lens' ParameterConstraints [Text]
pcAllowedValues = lens _pcAllowedValues (\s a -> s {_pcAllowedValues = a}) . _Default . _Coerce

instance FromJSON ParameterConstraints where
  parseJSON =
    withObject
      "ParameterConstraints"
      ( \x ->
          ParameterConstraints' <$> (x .:? "AllowedValues" .!= mempty)
      )

instance Hashable ParameterConstraints

instance NFData ParameterConstraints
