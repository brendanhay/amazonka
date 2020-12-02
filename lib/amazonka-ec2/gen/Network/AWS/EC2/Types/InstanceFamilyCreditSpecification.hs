{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceFamilyCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceFamilyCreditSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the default credit option for CPU usage of a burstable performance instance family.
--
--
--
-- /See:/ 'instanceFamilyCreditSpecification' smart constructor.
data InstanceFamilyCreditSpecification = InstanceFamilyCreditSpecification'
  { _ifcsInstanceFamily ::
      !( Maybe
           UnlimitedSupportedInstanceFamily
       ),
    _ifcsCPUCredits ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceFamilyCreditSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifcsInstanceFamily' - The instance family.
--
-- * 'ifcsCPUCredits' - The default credit option for CPU usage of the instance family. Valid values are @standard@ and @unlimited@ .
instanceFamilyCreditSpecification ::
  InstanceFamilyCreditSpecification
instanceFamilyCreditSpecification =
  InstanceFamilyCreditSpecification'
    { _ifcsInstanceFamily = Nothing,
      _ifcsCPUCredits = Nothing
    }

-- | The instance family.
ifcsInstanceFamily :: Lens' InstanceFamilyCreditSpecification (Maybe UnlimitedSupportedInstanceFamily)
ifcsInstanceFamily = lens _ifcsInstanceFamily (\s a -> s {_ifcsInstanceFamily = a})

-- | The default credit option for CPU usage of the instance family. Valid values are @standard@ and @unlimited@ .
ifcsCPUCredits :: Lens' InstanceFamilyCreditSpecification (Maybe Text)
ifcsCPUCredits = lens _ifcsCPUCredits (\s a -> s {_ifcsCPUCredits = a})

instance FromXML InstanceFamilyCreditSpecification where
  parseXML x =
    InstanceFamilyCreditSpecification'
      <$> (x .@? "instanceFamily") <*> (x .@? "cpuCredits")

instance Hashable InstanceFamilyCreditSpecification

instance NFData InstanceFamilyCreditSpecification
