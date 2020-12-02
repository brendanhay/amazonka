{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCreditSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCreditSpecificationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the credit option for CPU usage of a burstable performance instance.
--
--
--
-- /See:/ 'instanceCreditSpecificationRequest' smart constructor.
data InstanceCreditSpecificationRequest = InstanceCreditSpecificationRequest'
  { _icsrInstanceId ::
      !(Maybe Text),
    _icsrCPUCredits ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceCreditSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icsrInstanceId' - The ID of the instance.
--
-- * 'icsrCPUCredits' - The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
instanceCreditSpecificationRequest ::
  InstanceCreditSpecificationRequest
instanceCreditSpecificationRequest =
  InstanceCreditSpecificationRequest'
    { _icsrInstanceId = Nothing,
      _icsrCPUCredits = Nothing
    }

-- | The ID of the instance.
icsrInstanceId :: Lens' InstanceCreditSpecificationRequest (Maybe Text)
icsrInstanceId = lens _icsrInstanceId (\s a -> s {_icsrInstanceId = a})

-- | The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
icsrCPUCredits :: Lens' InstanceCreditSpecificationRequest (Maybe Text)
icsrCPUCredits = lens _icsrCPUCredits (\s a -> s {_icsrCPUCredits = a})

instance Hashable InstanceCreditSpecificationRequest

instance NFData InstanceCreditSpecificationRequest

instance ToQuery InstanceCreditSpecificationRequest where
  toQuery InstanceCreditSpecificationRequest' {..} =
    mconcat
      ["InstanceId" =: _icsrInstanceId, "CpuCredits" =: _icsrCPUCredits]
