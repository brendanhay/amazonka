{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreditSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the credit option for CPU usage of a T2, T3, or T3a instance.
--
--
--
-- /See:/ 'creditSpecification' smart constructor.
newtype CreditSpecification = CreditSpecification'
  { _csCPUCredits ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreditSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCPUCredits' - The credit option for CPU usage of a T2, T3, or T3a instance. Valid values are @standard@ and @unlimited@ .
creditSpecification ::
  CreditSpecification
creditSpecification = CreditSpecification' {_csCPUCredits = Nothing}

-- | The credit option for CPU usage of a T2, T3, or T3a instance. Valid values are @standard@ and @unlimited@ .
csCPUCredits :: Lens' CreditSpecification (Maybe Text)
csCPUCredits = lens _csCPUCredits (\s a -> s {_csCPUCredits = a})

instance FromXML CreditSpecification where
  parseXML x = CreditSpecification' <$> (x .@? "cpuCredits")

instance Hashable CreditSpecification

instance NFData CreditSpecification
