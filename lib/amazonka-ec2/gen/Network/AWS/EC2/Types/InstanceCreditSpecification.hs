{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCreditSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the credit option for CPU usage of a burstable performance instance.
--
--
--
-- /See:/ 'instanceCreditSpecification' smart constructor.
data InstanceCreditSpecification = InstanceCreditSpecification'
  { _icsInstanceId ::
      !(Maybe Text),
    _icsCPUCredits :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceCreditSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icsInstanceId' - The ID of the instance.
--
-- * 'icsCPUCredits' - The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
instanceCreditSpecification ::
  InstanceCreditSpecification
instanceCreditSpecification =
  InstanceCreditSpecification'
    { _icsInstanceId = Nothing,
      _icsCPUCredits = Nothing
    }

-- | The ID of the instance.
icsInstanceId :: Lens' InstanceCreditSpecification (Maybe Text)
icsInstanceId = lens _icsInstanceId (\s a -> s {_icsInstanceId = a})

-- | The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
icsCPUCredits :: Lens' InstanceCreditSpecification (Maybe Text)
icsCPUCredits = lens _icsCPUCredits (\s a -> s {_icsCPUCredits = a})

instance FromXML InstanceCreditSpecification where
  parseXML x =
    InstanceCreditSpecification'
      <$> (x .@? "instanceId") <*> (x .@? "cpuCredits")

instance Hashable InstanceCreditSpecification

instance NFData InstanceCreditSpecification
