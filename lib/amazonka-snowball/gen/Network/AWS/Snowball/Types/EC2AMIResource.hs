{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.EC2AMIResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.EC2AMIResource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A JSON-formatted object that contains the IDs for an Amazon Machine Image (AMI), including the Amazon EC2 AMI ID and the Snow device AMI ID. Each AMI has these two IDs to simplify identifying the AMI in both the AWS Cloud and on the device.
--
--
--
-- /See:/ 'ec2AMIResource' smart constructor.
data EC2AMIResource = EC2AMIResource'
  { _earSnowballAMIId ::
      !(Maybe Text),
    _earAMIId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2AMIResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'earSnowballAMIId' - The ID of the AMI on the Snow device.
--
-- * 'earAMIId' - The ID of the AMI in Amazon EC2.
ec2AMIResource ::
  -- | 'earAMIId'
  Text ->
  EC2AMIResource
ec2AMIResource pAMIId_ =
  EC2AMIResource' {_earSnowballAMIId = Nothing, _earAMIId = pAMIId_}

-- | The ID of the AMI on the Snow device.
earSnowballAMIId :: Lens' EC2AMIResource (Maybe Text)
earSnowballAMIId = lens _earSnowballAMIId (\s a -> s {_earSnowballAMIId = a})

-- | The ID of the AMI in Amazon EC2.
earAMIId :: Lens' EC2AMIResource Text
earAMIId = lens _earAMIId (\s a -> s {_earAMIId = a})

instance FromJSON EC2AMIResource where
  parseJSON =
    withObject
      "EC2AMIResource"
      ( \x ->
          EC2AMIResource' <$> (x .:? "SnowballAmiId") <*> (x .: "AmiId")
      )

instance Hashable EC2AMIResource

instance NFData EC2AMIResource

instance ToJSON EC2AMIResource where
  toJSON EC2AMIResource' {..} =
    object
      ( catMaybes
          [ ("SnowballAmiId" .=) <$> _earSnowballAMIId,
            Just ("AmiId" .= _earAMIId)
          ]
      )
