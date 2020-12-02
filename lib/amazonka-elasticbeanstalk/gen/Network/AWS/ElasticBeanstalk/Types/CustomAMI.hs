{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.CustomAMI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.CustomAMI where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A custom AMI available to platforms.
--
--
--
-- /See:/ 'customAMI' smart constructor.
data CustomAMI = CustomAMI'
  { _caVirtualizationType :: !(Maybe Text),
    _caImageId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomAMI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caVirtualizationType' - The type of virtualization used to create the custom AMI.
--
-- * 'caImageId' - THe ID of the image used to create the custom AMI.
customAMI ::
  CustomAMI
customAMI =
  CustomAMI' {_caVirtualizationType = Nothing, _caImageId = Nothing}

-- | The type of virtualization used to create the custom AMI.
caVirtualizationType :: Lens' CustomAMI (Maybe Text)
caVirtualizationType = lens _caVirtualizationType (\s a -> s {_caVirtualizationType = a})

-- | THe ID of the image used to create the custom AMI.
caImageId :: Lens' CustomAMI (Maybe Text)
caImageId = lens _caImageId (\s a -> s {_caImageId = a})

instance FromXML CustomAMI where
  parseXML x =
    CustomAMI' <$> (x .@? "VirtualizationType") <*> (x .@? "ImageId")

instance Hashable CustomAMI

instance NFData CustomAMI
