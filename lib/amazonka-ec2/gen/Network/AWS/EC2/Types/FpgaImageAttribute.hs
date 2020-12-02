{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImageAttribute where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LoadPermission
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Amazon FPGA image (AFI) attribute.
--
--
--
-- /See:/ 'fpgaImageAttribute' smart constructor.
data FpgaImageAttribute = FpgaImageAttribute'
  { _fiaFpgaImageId ::
      !(Maybe Text),
    _fiaName :: !(Maybe Text),
    _fiaProductCodes :: !(Maybe [ProductCode]),
    _fiaDescription :: !(Maybe Text),
    _fiaLoadPermissions :: !(Maybe [LoadPermission])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FpgaImageAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fiaFpgaImageId' - The ID of the AFI.
--
-- * 'fiaName' - The name of the AFI.
--
-- * 'fiaProductCodes' - The product codes.
--
-- * 'fiaDescription' - The description of the AFI.
--
-- * 'fiaLoadPermissions' - The load permissions.
fpgaImageAttribute ::
  FpgaImageAttribute
fpgaImageAttribute =
  FpgaImageAttribute'
    { _fiaFpgaImageId = Nothing,
      _fiaName = Nothing,
      _fiaProductCodes = Nothing,
      _fiaDescription = Nothing,
      _fiaLoadPermissions = Nothing
    }

-- | The ID of the AFI.
fiaFpgaImageId :: Lens' FpgaImageAttribute (Maybe Text)
fiaFpgaImageId = lens _fiaFpgaImageId (\s a -> s {_fiaFpgaImageId = a})

-- | The name of the AFI.
fiaName :: Lens' FpgaImageAttribute (Maybe Text)
fiaName = lens _fiaName (\s a -> s {_fiaName = a})

-- | The product codes.
fiaProductCodes :: Lens' FpgaImageAttribute [ProductCode]
fiaProductCodes = lens _fiaProductCodes (\s a -> s {_fiaProductCodes = a}) . _Default . _Coerce

-- | The description of the AFI.
fiaDescription :: Lens' FpgaImageAttribute (Maybe Text)
fiaDescription = lens _fiaDescription (\s a -> s {_fiaDescription = a})

-- | The load permissions.
fiaLoadPermissions :: Lens' FpgaImageAttribute [LoadPermission]
fiaLoadPermissions = lens _fiaLoadPermissions (\s a -> s {_fiaLoadPermissions = a}) . _Default . _Coerce

instance FromXML FpgaImageAttribute where
  parseXML x =
    FpgaImageAttribute'
      <$> (x .@? "fpgaImageId")
      <*> (x .@? "name")
      <*> (x .@? "productCodes" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "description")
      <*> (x .@? "loadPermissions" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable FpgaImageAttribute

instance NFData FpgaImageAttribute
