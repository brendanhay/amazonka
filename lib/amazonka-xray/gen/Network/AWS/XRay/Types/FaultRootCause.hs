{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultRootCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultRootCause where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.FaultRootCauseService

-- | The root cause information for a trace summary fault.
--
--
--
-- /See:/ 'faultRootCause' smart constructor.
data FaultRootCause = FaultRootCause'
  { _frcClientImpacting ::
      !(Maybe Bool),
    _frcServices :: !(Maybe [FaultRootCauseService])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FaultRootCause' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frcClientImpacting' - A flag that denotes that the root cause impacts the trace client.
--
-- * 'frcServices' - A list of corresponding services. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
faultRootCause ::
  FaultRootCause
faultRootCause =
  FaultRootCause'
    { _frcClientImpacting = Nothing,
      _frcServices = Nothing
    }

-- | A flag that denotes that the root cause impacts the trace client.
frcClientImpacting :: Lens' FaultRootCause (Maybe Bool)
frcClientImpacting = lens _frcClientImpacting (\s a -> s {_frcClientImpacting = a})

-- | A list of corresponding services. A service identifies a segment and it contains a name, account ID, type, and inferred flag.
frcServices :: Lens' FaultRootCause [FaultRootCauseService]
frcServices = lens _frcServices (\s a -> s {_frcServices = a}) . _Default . _Coerce

instance FromJSON FaultRootCause where
  parseJSON =
    withObject
      "FaultRootCause"
      ( \x ->
          FaultRootCause'
            <$> (x .:? "ClientImpacting") <*> (x .:? "Services" .!= mempty)
      )

instance Hashable FaultRootCause

instance NFData FaultRootCause
