{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ProductCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ProductCode where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the product code for the EC2 instance.
--
--
--
-- /See:/ 'productCode' smart constructor.
data ProductCode = ProductCode'
  { _pcProductType :: !(Maybe Text),
    _pcCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProductCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcProductType' - The product code type.
--
-- * 'pcCode' - The product code information.
productCode ::
  ProductCode
productCode =
  ProductCode' {_pcProductType = Nothing, _pcCode = Nothing}

-- | The product code type.
pcProductType :: Lens' ProductCode (Maybe Text)
pcProductType = lens _pcProductType (\s a -> s {_pcProductType = a})

-- | The product code information.
pcCode :: Lens' ProductCode (Maybe Text)
pcCode = lens _pcCode (\s a -> s {_pcCode = a})

instance FromJSON ProductCode where
  parseJSON =
    withObject
      "ProductCode"
      (\x -> ProductCode' <$> (x .:? "productType") <*> (x .:? "code"))

instance Hashable ProductCode

instance NFData ProductCode
