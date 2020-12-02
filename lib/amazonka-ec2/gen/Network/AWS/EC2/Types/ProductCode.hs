{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ProductCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ProductCode where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ProductCodeValues
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a product code.
--
--
--
-- /See:/ 'productCode' smart constructor.
data ProductCode = ProductCode'
  { _pcProductCodeType ::
      !(Maybe ProductCodeValues),
    _pcProductCodeId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProductCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcProductCodeType' - The type of product code.
--
-- * 'pcProductCodeId' - The product code.
productCode ::
  ProductCode
productCode =
  ProductCode'
    { _pcProductCodeType = Nothing,
      _pcProductCodeId = Nothing
    }

-- | The type of product code.
pcProductCodeType :: Lens' ProductCode (Maybe ProductCodeValues)
pcProductCodeType = lens _pcProductCodeType (\s a -> s {_pcProductCodeType = a})

-- | The product code.
pcProductCodeId :: Lens' ProductCode (Maybe Text)
pcProductCodeId = lens _pcProductCodeId (\s a -> s {_pcProductCodeId = a})

instance FromXML ProductCode where
  parseXML x =
    ProductCode' <$> (x .@? "type") <*> (x .@? "productCode")

instance Hashable ProductCode

instance NFData ProductCode
