{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ParameterNameValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ParameterNameValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a name-value pair that is used to update the value of a parameter.
--
--
--
-- /See:/ 'parameterNameValue' smart constructor.
data ParameterNameValue = ParameterNameValue'
  { _pnvParameterValue ::
      !(Maybe Text),
    _pnvParameterName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterNameValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pnvParameterValue' - The value of the parameter.
--
-- * 'pnvParameterName' - The name of the parameter.
parameterNameValue ::
  ParameterNameValue
parameterNameValue =
  ParameterNameValue'
    { _pnvParameterValue = Nothing,
      _pnvParameterName = Nothing
    }

-- | The value of the parameter.
pnvParameterValue :: Lens' ParameterNameValue (Maybe Text)
pnvParameterValue = lens _pnvParameterValue (\s a -> s {_pnvParameterValue = a})

-- | The name of the parameter.
pnvParameterName :: Lens' ParameterNameValue (Maybe Text)
pnvParameterName = lens _pnvParameterName (\s a -> s {_pnvParameterName = a})

instance Hashable ParameterNameValue

instance NFData ParameterNameValue

instance ToQuery ParameterNameValue where
  toQuery ParameterNameValue' {..} =
    mconcat
      [ "ParameterValue" =: _pnvParameterValue,
        "ParameterName" =: _pnvParameterName
      ]
