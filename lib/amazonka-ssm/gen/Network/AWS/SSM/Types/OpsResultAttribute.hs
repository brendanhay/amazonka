{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsResultAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsResultAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The OpsItem data type to return.
--
--
--
-- /See:/ 'opsResultAttribute' smart constructor.
newtype OpsResultAttribute = OpsResultAttribute'
  { _oraTypeName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpsResultAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oraTypeName' - Name of the data type. Valid value: AWS:OpsItem, AWS:EC2InstanceInformation, AWS:OpsItemTrendline, or AWS:ComplianceSummary.
opsResultAttribute ::
  -- | 'oraTypeName'
  Text ->
  OpsResultAttribute
opsResultAttribute pTypeName_ =
  OpsResultAttribute' {_oraTypeName = pTypeName_}

-- | Name of the data type. Valid value: AWS:OpsItem, AWS:EC2InstanceInformation, AWS:OpsItemTrendline, or AWS:ComplianceSummary.
oraTypeName :: Lens' OpsResultAttribute Text
oraTypeName = lens _oraTypeName (\s a -> s {_oraTypeName = a})

instance Hashable OpsResultAttribute

instance NFData OpsResultAttribute

instance ToJSON OpsResultAttribute where
  toJSON OpsResultAttribute' {..} =
    object (catMaybes [Just ("TypeName" .= _oraTypeName)])
