{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImage where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FpgaImageState
import Network.AWS.EC2.Types.PciId
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Amazon FPGA image (AFI).
--
--
--
-- /See:/ 'fpgaImage' smart constructor.
data FpgaImage = FpgaImage'
  { _fiShellVersion :: !(Maybe Text),
    _fiPciId :: !(Maybe PciId),
    _fiState :: !(Maybe FpgaImageState),
    _fiOwnerAlias :: !(Maybe Text),
    _fiFpgaImageId :: !(Maybe Text),
    _fiDataRetentionSupport :: !(Maybe Bool),
    _fiOwnerId :: !(Maybe Text),
    _fiUpdateTime :: !(Maybe ISO8601),
    _fiName :: !(Maybe Text),
    _fiProductCodes :: !(Maybe [ProductCode]),
    _fiDescription :: !(Maybe Text),
    _fiCreateTime :: !(Maybe ISO8601),
    _fiTags :: !(Maybe [Tag]),
    _fiPublic :: !(Maybe Bool),
    _fiFpgaImageGlobalId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FpgaImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fiShellVersion' - The version of the AWS Shell that was used to create the bitstream.
--
-- * 'fiPciId' - Information about the PCI bus.
--
-- * 'fiState' - Information about the state of the AFI.
--
-- * 'fiOwnerAlias' - The alias of the AFI owner. Possible values include @self@ , @amazon@ , and @aws-marketplace@ .
--
-- * 'fiFpgaImageId' - The FPGA image identifier (AFI ID).
--
-- * 'fiDataRetentionSupport' - Indicates whether data retention support is enabled for the AFI.
--
-- * 'fiOwnerId' - The AWS account ID of the AFI owner.
--
-- * 'fiUpdateTime' - The time of the most recent update to the AFI.
--
-- * 'fiName' - The name of the AFI.
--
-- * 'fiProductCodes' - The product codes for the AFI.
--
-- * 'fiDescription' - The description of the AFI.
--
-- * 'fiCreateTime' - The date and time the AFI was created.
--
-- * 'fiTags' - Any tags assigned to the AFI.
--
-- * 'fiPublic' - Indicates whether the AFI is public.
--
-- * 'fiFpgaImageGlobalId' - The global FPGA image identifier (AGFI ID).
fpgaImage ::
  FpgaImage
fpgaImage =
  FpgaImage'
    { _fiShellVersion = Nothing,
      _fiPciId = Nothing,
      _fiState = Nothing,
      _fiOwnerAlias = Nothing,
      _fiFpgaImageId = Nothing,
      _fiDataRetentionSupport = Nothing,
      _fiOwnerId = Nothing,
      _fiUpdateTime = Nothing,
      _fiName = Nothing,
      _fiProductCodes = Nothing,
      _fiDescription = Nothing,
      _fiCreateTime = Nothing,
      _fiTags = Nothing,
      _fiPublic = Nothing,
      _fiFpgaImageGlobalId = Nothing
    }

-- | The version of the AWS Shell that was used to create the bitstream.
fiShellVersion :: Lens' FpgaImage (Maybe Text)
fiShellVersion = lens _fiShellVersion (\s a -> s {_fiShellVersion = a})

-- | Information about the PCI bus.
fiPciId :: Lens' FpgaImage (Maybe PciId)
fiPciId = lens _fiPciId (\s a -> s {_fiPciId = a})

-- | Information about the state of the AFI.
fiState :: Lens' FpgaImage (Maybe FpgaImageState)
fiState = lens _fiState (\s a -> s {_fiState = a})

-- | The alias of the AFI owner. Possible values include @self@ , @amazon@ , and @aws-marketplace@ .
fiOwnerAlias :: Lens' FpgaImage (Maybe Text)
fiOwnerAlias = lens _fiOwnerAlias (\s a -> s {_fiOwnerAlias = a})

-- | The FPGA image identifier (AFI ID).
fiFpgaImageId :: Lens' FpgaImage (Maybe Text)
fiFpgaImageId = lens _fiFpgaImageId (\s a -> s {_fiFpgaImageId = a})

-- | Indicates whether data retention support is enabled for the AFI.
fiDataRetentionSupport :: Lens' FpgaImage (Maybe Bool)
fiDataRetentionSupport = lens _fiDataRetentionSupport (\s a -> s {_fiDataRetentionSupport = a})

-- | The AWS account ID of the AFI owner.
fiOwnerId :: Lens' FpgaImage (Maybe Text)
fiOwnerId = lens _fiOwnerId (\s a -> s {_fiOwnerId = a})

-- | The time of the most recent update to the AFI.
fiUpdateTime :: Lens' FpgaImage (Maybe UTCTime)
fiUpdateTime = lens _fiUpdateTime (\s a -> s {_fiUpdateTime = a}) . mapping _Time

-- | The name of the AFI.
fiName :: Lens' FpgaImage (Maybe Text)
fiName = lens _fiName (\s a -> s {_fiName = a})

-- | The product codes for the AFI.
fiProductCodes :: Lens' FpgaImage [ProductCode]
fiProductCodes = lens _fiProductCodes (\s a -> s {_fiProductCodes = a}) . _Default . _Coerce

-- | The description of the AFI.
fiDescription :: Lens' FpgaImage (Maybe Text)
fiDescription = lens _fiDescription (\s a -> s {_fiDescription = a})

-- | The date and time the AFI was created.
fiCreateTime :: Lens' FpgaImage (Maybe UTCTime)
fiCreateTime = lens _fiCreateTime (\s a -> s {_fiCreateTime = a}) . mapping _Time

-- | Any tags assigned to the AFI.
fiTags :: Lens' FpgaImage [Tag]
fiTags = lens _fiTags (\s a -> s {_fiTags = a}) . _Default . _Coerce

-- | Indicates whether the AFI is public.
fiPublic :: Lens' FpgaImage (Maybe Bool)
fiPublic = lens _fiPublic (\s a -> s {_fiPublic = a})

-- | The global FPGA image identifier (AGFI ID).
fiFpgaImageGlobalId :: Lens' FpgaImage (Maybe Text)
fiFpgaImageGlobalId = lens _fiFpgaImageGlobalId (\s a -> s {_fiFpgaImageGlobalId = a})

instance FromXML FpgaImage where
  parseXML x =
    FpgaImage'
      <$> (x .@? "shellVersion")
      <*> (x .@? "pciId")
      <*> (x .@? "state")
      <*> (x .@? "ownerAlias")
      <*> (x .@? "fpgaImageId")
      <*> (x .@? "dataRetentionSupport")
      <*> (x .@? "ownerId")
      <*> (x .@? "updateTime")
      <*> (x .@? "name")
      <*> (x .@? "productCodes" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "description")
      <*> (x .@? "createTime")
      <*> (x .@? "tags" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "public")
      <*> (x .@? "fpgaImageGlobalId")

instance Hashable FpgaImage

instance NFData FpgaImage
