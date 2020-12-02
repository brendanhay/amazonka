{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.CompatibleImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.CompatibleImage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A JSON-formatted object that describes a compatible Amazon Machine Image (AMI), including the ID and name for a Snow device AMI. This AMI is compatible with the device's physical hardware requirements, and it should be able to be run in an SBE1 instance on the device.
--
--
--
-- /See:/ 'compatibleImage' smart constructor.
data CompatibleImage = CompatibleImage'
  { _ciName :: !(Maybe Text),
    _ciAMIId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CompatibleImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciName' - The optional name of a compatible image.
--
-- * 'ciAMIId' - The unique identifier for an individual Snow device AMI.
compatibleImage ::
  CompatibleImage
compatibleImage =
  CompatibleImage' {_ciName = Nothing, _ciAMIId = Nothing}

-- | The optional name of a compatible image.
ciName :: Lens' CompatibleImage (Maybe Text)
ciName = lens _ciName (\s a -> s {_ciName = a})

-- | The unique identifier for an individual Snow device AMI.
ciAMIId :: Lens' CompatibleImage (Maybe Text)
ciAMIId = lens _ciAMIId (\s a -> s {_ciAMIId = a})

instance FromJSON CompatibleImage where
  parseJSON =
    withObject
      "CompatibleImage"
      (\x -> CompatibleImage' <$> (x .:? "Name") <*> (x .:? "AmiId"))

instance Hashable CompatibleImage

instance NFData CompatibleImage
