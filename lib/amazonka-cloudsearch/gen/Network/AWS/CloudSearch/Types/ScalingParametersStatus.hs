{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ScalingParametersStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ScalingParametersStatus where

import Network.AWS.CloudSearch.Types.OptionStatus
import Network.AWS.CloudSearch.Types.ScalingParameters
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status and configuration of a search domain's scaling parameters.
--
--
--
-- /See:/ 'scalingParametersStatus' smart constructor.
data ScalingParametersStatus = ScalingParametersStatus'
  { _spsOptions ::
      !ScalingParameters,
    _spsStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingParametersStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spsOptions' - Undocumented member.
--
-- * 'spsStatus' - Undocumented member.
scalingParametersStatus ::
  -- | 'spsOptions'
  ScalingParameters ->
  -- | 'spsStatus'
  OptionStatus ->
  ScalingParametersStatus
scalingParametersStatus pOptions_ pStatus_ =
  ScalingParametersStatus'
    { _spsOptions = pOptions_,
      _spsStatus = pStatus_
    }

-- | Undocumented member.
spsOptions :: Lens' ScalingParametersStatus ScalingParameters
spsOptions = lens _spsOptions (\s a -> s {_spsOptions = a})

-- | Undocumented member.
spsStatus :: Lens' ScalingParametersStatus OptionStatus
spsStatus = lens _spsStatus (\s a -> s {_spsStatus = a})

instance FromXML ScalingParametersStatus where
  parseXML x =
    ScalingParametersStatus' <$> (x .@ "Options") <*> (x .@ "Status")

instance Hashable ScalingParametersStatus

instance NFData ScalingParametersStatus
